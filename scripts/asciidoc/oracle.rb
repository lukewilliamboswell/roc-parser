#!/usr/bin/env ruby
# Persistent deterministic JSONL protocol. Caller paths name virtual sources only.
require 'json'
require 'rubygems'
gem 'asciidoctor', '= 2.0.23'
require 'asciidoctor'
require 'asciidoctor/logging'
require 'asciidoctor/extensions'
require 'pathname'
require 'tmpdir'
require 'fileutils'
MAX_SOURCE = 262144
MAX_LINE = 1600000

# SECURE mode disables ambient include/URI reads, docinfo and external syntax
# highlighters. The only optional extension reads a request-local source map.
def projection(node)
  blocks = node.respond_to?(:blocks) ? node.blocks : []
  blocks = blocks.flatten.compact.flat_map { |child| child.context == :preamble ? child.blocks : [child] }
  { 'context' => node.context.to_s, 'level' => node.context == :section ? node.level : 0,
    'title' => node.title || '', 'source' => node.context == :section ? '' : (node.respond_to?(:source) ? node.source.to_s : ''),
    'blocks' => blocks.map { |child| projection(child) } }
end

def scalar(value)
  case value
  when String, Integer, Float, TrueClass, FalseClass, NilClass then value
  when Symbol then value.to_s
  when Array then value.map { |item| scalar(item) }
  when Hash then value.keys.map(&:to_s).sort.each_with_object({}) { |key, result| result[key] = scalar(value.key?(key) ? value[key] : value[key.to_sym]) }
  else value.class.to_s
  end
end

def ast(node)
  return nil if node.nil? # A description-list term can have no definition.
  result = { 'context' => node.context.to_s, 'id' => node.id, 'title' => node.title,
             'attributes' => scalar(node.attributes), 'style' => node.style,
             'roles' => node.roles, 'level' => node.respond_to?(:level) ? node.level : nil }
  result['source'] = node.source if node.respond_to?(:source)
  result['subs'] = scalar(node.subs) if node.respond_to?(:subs)
  result['text'] = node.text if node.context == :list_item
  result['target'] = node.target if node.respond_to?(:target)
  result['blocks'] = node.blocks.flatten.compact.map { |child| ast(child) } if node.respond_to?(:blocks)
  if node.respond_to?(:items)
    result['items'] = node.items.map { |item| item.is_a?(Array) ? item.map { |part| part.is_a?(Array) ? part.map { |entry| ast(entry) } : ast(part) } : ast(item) }
  end
  if node.context == :table
    result['rows'] = node.rows.to_h.transform_values { |rows| rows.map { |row| row.map { |cell| { 'text' => cell.text, 'style' => cell.style, 'attributes' => scalar(cell.attributes) } } } }
    result['columns'] = node.columns.map { |column| scalar(column.attributes) }
  end
  result
end

# An explicit per-document extension. SECURE mode remains enabled: unmatched
# directives can never fall back to disk/URI access. Content comes only from
# caller-provided strings; no filesystem adapter or global registry is used.
class VirtualIncludePolicyError < StandardError; end

class VirtualIncludeReader < Asciidoctor::PreprocessorReader
  MAX_DEPTH = 16
  MAX_EXPANSION = 1048576

  def initialize(document, source, sources, root)
    @sources = sources
    @root = root
    @state = { expanded: 0 }
    super(document, source, nil, normalize: true)
    @maxdepth = { abs: MAX_DEPTH, curr: MAX_DEPTH, rel: MAX_DEPTH }
  end

  # Run the pinned engine's actual include implementation, including its line
  # and tag selectors. Only its include read is temporarily permitted; path
  # resolution below is confined to exact keys in the supplied source map.
  # All other document processing retains SECURE mode. The temporary directory
  # is private, contains only supplied regular files, and is removed per request.
  def preprocess_include_directive(target, attrlist)
    raise VirtualIncludePolicyError, 'maximum virtual include depth exceeded (16)' if @include_stack.size >= MAX_DEPTH
    safe = @document.instance_variable_get(:@safe)
    @document.instance_variable_set(:@safe, Asciidoctor::SafeMode::SAFE)
    super
  ensure
    @document.instance_variable_set(:@safe, safe) if safe
  end

  def resolve_include_path(target, _attrlist, _attributes)
    raise VirtualIncludePolicyError, 'virtual includes prohibit absolute paths and URIs' if target.start_with?('/', '\\') || target.match?(/\A[A-Za-z][A-Za-z0-9+.-]*:/) || target.include?('\\')
    current = @path == '<stdin>' ? '.' : File.dirname(@path)
    path = Pathname.new(File.join(current, target)).cleanpath.to_s.sub(/\A\.\//, '')
    raise VirtualIncludePolicyError, 'virtual include escapes root' if path == '..' || path.start_with?('../')
    raise VirtualIncludePolicyError, "virtual include not supplied: #{path}" unless @sources.key?(path)
    active = @include_stack.map { |entry| entry[3] } + [@path]
    raise VirtualIncludePolicyError, "virtual include cycle: #{path}" if active.include?(path)
    @state[:expanded] += @sources[path].bytesize
    raise VirtualIncludePolicyError, 'virtual include expansion byte limit exceeded' if @state[:expanded] > MAX_EXPANSION
    [File.join(@root, path), :file, path]
  end
end

STDOUT.sync = true
while (line = STDIN.gets(MAX_LINE + 1))
  request = nil
  virtual_root = nil
  begin
    raise ArgumentError, 'request exceeds line limit' if line.bytesize > MAX_LINE || !line.end_with?("\n")
    request = JSON.parse(line)
    raise ArgumentError, 'expected id, source and optional includes' unless request.is_a?(Hash) && (request.keys.sort == %w[id source] || request.keys.sort == %w[id includes source])
    raise ArgumentError, 'invalid request id' unless request['id'].is_a?(Integer) && request['id'] >= 0
    source = request['source']
    raise ArgumentError, 'source must be UTF-8 text within limit' unless source.is_a?(String) && source.valid_encoding? && source.bytesize <= MAX_SOURCE
    sources = request['includes']
    registry = false
    if request.key?('includes')
      raise ArgumentError, 'includes must be a map of at most 128 bounded UTF-8 sources' unless sources.is_a?(Hash) && sources.size <= 128 && sources.all? { |path, text| path.is_a?(String) && path.bytesize <= 1024 && path.match?(/\A[A-Za-z0-9_.\/-]+\z/) && !path.start_with?('/') && !path.split('/').include?('..') && Pathname.new(path).cleanpath.to_s == path && text.is_a?(String) && text.valid_encoding? } && source.bytesize + sources.sum { |path, text| path.bytesize + text.bytesize } <= MAX_SOURCE
      virtual_root = Dir.mktmpdir('roc-asciidoc-oracle-')
      sources.each do |path, content|
        destination = File.join(virtual_root, path)
        FileUtils.mkdir_p(File.dirname(destination))
        File.binwrite(destination, content)
      end
    end
    logger = Asciidoctor::MemoryLogger.new
    # Recreate document and logger per request: attributes never leak between jobs.
    document = Asciidoctor.load(source, safe: :secure, backend: 'html5', header_footer: false,
      standalone: false, sourcemap: false, parse: false, extensions: false, extension_registry: registry, logger: logger,
      attributes: { 'docdate' => '2000-01-01', 'docyear' => '2000', 'doctime' => '00:00:00 UTC',
                    'docdatetime' => '2000-01-01 00:00:00 UTC', 'localdate' => '2000-01-01',
                    'localyear' => '2000', 'localtime' => '00:00:00 UTC', 'localdatetime' => '2000-01-01 00:00:00 UTC',
                    'attribute-missing' => 'skip', 'docdir' => '.', 'outdir' => '.',
                    'max-include-depth' => 0, 'allow-uri-read!' => '', 'stylesheet!' => '',
                    'source-highlighter!' => '', 'icons!' => '', 'webfonts!' => '' })
    document.instance_variable_set(:@reader, VirtualIncludeReader.new(document, source, sources, virtual_root)) if sources
    document.parse
    projected = projection(document)
    html = document.convert
    warnings = logger.messages.map do |message|
      detail = message[:message]
      warning = { 'severity' => message[:severity].to_s, 'message' => detail.is_a?(Hash) ? detail[:text].to_s : detail.to_s }
      if detail.is_a?(Hash) && (cursor = detail[:source_location])
        warning['source_location'] = { 'path' => cursor.path.to_s, 'line' => cursor.lineno }
      end
      warning
    end
    puts JSON.generate({ 'id' => request['id'], 'status' => 'ok', 'version' => Asciidoctor::VERSION,
      'projection' => { 'title' => document.header? ? document.doctitle.to_s : '', 'blocks' => projected['blocks'] },
      'ast' => ast(document), 'html' => html, 'warnings' => warnings,
      'include_policy' => sources ? 'virtual-map-secure' : 'secure-no-expansion',
      'unsupported' => source.match?(/^include::/) && !sources ? ['include resolution disabled by secure oracle policy'] : [] })
  rescue StandardError => error
    puts JSON.generate({ 'id' => request.is_a?(Hash) ? request['id'] : nil, 'status' => 'error', 'error' => error.class.to_s, 'error_kind' => error.is_a?(VirtualIncludePolicyError) ? 'virtual_include_policy' : 'engine_or_protocol', 'message' => error.message })
  ensure
    FileUtils.remove_entry(virtual_root) if virtual_root
  end
end
