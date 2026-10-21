#!/usr/bin/env ruby
# Persistent pinned reference. Reuse the production oracle's definitions before
# its protocol loop, so projection and confined native include semantics agree.
require 'json'
require 'digest'
require 'stringio'
reference = File.join(__dir__, 'oracle.rb')
definitions = File.read(reference).split(/^STDOUT.sync = true$/, 2)
raise 'oracle protocol boundary missing' unless definitions.length == 2
eval(definitions.first, TOPLEVEL_BINDING, reference, 1)

gem_root = Gem.loaded_specs.fetch('asciidoctor').full_gem_path
gem_digest = Digest::SHA256.new
Dir.glob(File.join(gem_root, 'lib/**/*.rb')).sort.each { |path| gem_digest.update(path.delete_prefix(gem_root) + "\0"); gem_digest.update(File.binread(path)) }
REFERENCE_CODE_HASH = gem_digest.hexdigest.freeze

# The native include selector implementation requests File.open. Substitute
# only exact caller-map paths with fresh memory streams; no source-map file
# creation, file reads, or path stat checks enter timed parsing. The production
# VirtualIncludeReader still enforces path, cycle, depth and byte policies.
module CallerMapMemoryReads
 def open(path, *arguments, **keywords, &block)
  sources = Thread.current[:roc_adoc_memory_sources]
  return super unless sources && sources.key?(path.to_s)
  stream = StringIO.new(sources.fetch(path.to_s), arguments.first || 'r')
  return stream unless block
  begin
   block.call(stream)
  ensure
   stream.close
  end
 end
end
File.singleton_class.prepend(CallerMapMemoryReads)
MEMORY_ROOT = '/__roc_asciidoc_performance_memory__'.freeze

FIXED_ATTRIBUTES = {
 'docdate'=>'2000-01-01','docyear'=>'2000','doctime'=>'00:00:00 UTC','docdatetime'=>'2000-01-01 00:00:00 UTC',
 'localdate'=>'2000-01-01','localyear'=>'2000','localtime'=>'00:00:00 UTC','localdatetime'=>'2000-01-01 00:00:00 UTC',
 'attribute-missing'=>'skip','docdir'=>'.','outdir'=>'.','max-include-depth'=>0,'allow-uri-read!'=>'',
 'stylesheet!'=>'','source-highlighter!'=>'','icons!'=>'','webfonts!'=>''
}.freeze

# API options seed the document; Roc permits later source redefinitions.
# Asciidoctor's trailing @ marks a soft initial value (including literal @).
# Applying the ordered entries in sequence preserves last duplicate wins.
def request_attributes(item)
 entries = item.fetch('attributes', [])
 raise 'invalid attributes' unless entries.is_a?(Array) && entries.all? { |entry| entry.is_a?(Hash) && entry.keys.sort == %w[name value] && entry['name'].is_a?(String) && entry['value'].is_a?(String) }
 return FIXED_ATTRIBUTES if entries.empty?
 entries.each_with_object(FIXED_ATTRIBUTES.dup) { |entry, attrs| attrs[entry.fetch('name')] = entry.fetch('value') + '@' }.freeze
end

def load_document(source, sources, root, attributes)
 logger = Asciidoctor::MemoryLogger.new
 document = Asciidoctor.load(source, safe: :secure, backend: 'html5', header_footer: false, standalone: false,
  sourcemap: false, parse: false, extensions: false, extension_registry: false, logger: logger, attributes: attributes)
 document.instance_variable_set(:@reader, VirtualIncludeReader.new(document, source, sources, root)) if sources
 document.parse
 document
end

def operation(source, sources, root, mode, attributes)
 document = load_document(source, sources, root, attributes)
 mode == 'render' ? document.convert.bytesize + 1 : document.blocks.length + document.doctitle.to_s.bytesize + 1
end
STDOUT.sync = true
while (line = STDIN.gets(16000001))
 root = nil
 begin
  raise 'incomplete or oversized request' unless line.end_with?("\n") && line.bytesize <= 16000000
  item = JSON.parse(line)
  source = item.fetch('source'); iterations = item.fetch('iterations'); warmup = item.fetch('warmup'); mode = item.fetch('mode')
  raise 'invalid request' unless source.is_a?(String) && source.valid_encoding? && source.bytesize <= 1048576 && iterations.is_a?(Integer) && (0..100000).cover?(iterations) && warmup.is_a?(Integer) && (0..10000).cover?(warmup) && %w[parse render].include?(mode)
  attributes = request_attributes(item)
  sources = item['expand'] ? item.fetch('includes').to_h { |entry| [entry.fetch('path'), entry.fetch('source')] } : nil
  if sources
   root = MEMORY_ROOT
   memory_sources = {}
   sources.each do |path, content|
    raise 'unsafe source-map path' unless path.match?(/\A[A-Za-z0-9_.\/-]+\z/) && !path.start_with?('/') && !path.split('/').include?('..') && Pathname.new(path).cleanpath.to_s == path
    memory_sources[File.join(root,path)] = content
   end
   Thread.current[:roc_adoc_memory_sources] = memory_sources.freeze
  end
  checksum = 0
  warmup.times { checksum += operation(source,sources,root,mode,attributes) }
  start = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
  iterations.times { checksum += operation(source,sources,root,mode,attributes) }
  elapsed = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond) - start
  response = {version:2,phase:iterations.zero? ? 'correctness' : 'timing',mode:mode,id:item['id'],iterations:iterations,warmup:warmup,successes:iterations,checksum:checksum,elapsed_ns:elapsed,
   oracle_version:Asciidoctor::VERSION,oracle_code_sha256:REFERENCE_CODE_HASH,include_io:'caller-map-memory',include_path_metadata:'native path normalization and policy; no filesystem stat/read'}
  if iterations.zero?
   document = load_document(source,sources,root,attributes)
   projected = projection(document)
   response[:projection] = {title:document.header? ? document.doctitle.to_s : '',blocks:projected['blocks']}
   response[:html] = document.convert
   # A separate fresh document computes the exact per-engine timed metric;
   # never call an extra doctitle/convert on the correctness document.
   response[:operation_checksum] = operation(source,sources,root,mode,attributes)
  end
  puts JSON.generate(response, max_nesting: false)
 rescue StandardError => error
  puts JSON.generate(error:error.class.to_s,message:error.message,id:item && item['id'])
 ensure
  # Correctness-only roots must not survive into subsequent timed requests.
  document = projected = response = nil
  Thread.current[:roc_adoc_memory_sources] = nil
  memory_sources = nil
 end
end
