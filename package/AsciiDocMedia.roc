import AsciiDocSource

## Pure HTML5 conversion of AsciiDoc block image, video and audio nodes.
## The caller supplies document attributes together with block attributes,
## and a rendered (and, for images, captioned) title.
AsciiDocMedia :: [].{
	render : { context : Str, target : Str, title : Str, id : Str, attrs : List({ name : Str, value : Str }) } -> Str
	render = |node| render_media(node, Legacy(node.attrs))

	## Internal prepared-renderer entry. The supplied index retains the first
	## binding for every source and local media attribute, exactly as render.
	render_indexed : { context : Str, target : Str, title : Str, id : Str, attrs : List({ name : Str, value : Str }) }, Dict(Str, Str) -> Str
	render_indexed = |node, first| render_media(node, Indexed(first))

	## Leading local attributes win over shared document bindings; trailing
	## local attributes remain the final source fallback, matching flat render.
	render_view : { context : Str, target : Str, title : Str, id : Str, attrs : List({ name : Str, value : Str }) }, { first : AsciiDocSource.Index, before : List({ name : Str, value : Str }), after : List({ name : Str, value : Str }) } -> Str
	render_view = |node, view| render_media(node, Layered(view))
}

MediaAttributes : [Legacy(List({ name : Str, value : Str })), Indexed(Dict(Str, Str)), Layered({ first : AsciiDocSource.Index, before : List({ name : Str, value : Str }), after : List({ name : Str, value : Str }) })]

render_media : { context : Str, target : Str, title : Str, id : Str, attrs : List({ name : Str, value : Str }) }, MediaAttributes -> Str
render_media = |node, attrs| {
	id = if node.id == "" "" else " id=\"${encode(node.id)}\""
	role = get(attrs, "role") ?? ""
	role_class = if role == "" "" else " ${role}"
	float = match get(attrs, "float") {
		Ok(value) => " ${value}"
		Err(_) => ""
	}
	align = match get(attrs, "align") {
		Ok(value) => " text-${value}"
		Err(_) => ""
	}
	classes = "${node.context}block${if node.context == "audio" "" else "${float}${align}"}${role_class}"
	width = dimension(attrs, "width", "2")
	height = dimension(attrs, "height", "3")
	title = if node.title == "" "" else "\n<div class=\"title\">${node.title}</div>"
	body = match node.context {
		"image" => {
			positional_alt = get(attrs, "1") ?? ""
			alt = get(attrs, "alt") ?? (if positional_alt == "" default_alt(node.target) else positional_alt)
			src = uri(node.target, attrs)
			img = "<img src=\"${encode(src)}\" alt=\"${encode(alt)}\"${width}${height}>"
			content = match get(attrs, "link") {
				Ok(link) => "<a class=\"image\" href=\"${encode(link)}\"${link_constraints(attrs)}>${img}</a>"
				Err(_) => img
			}
			"<div${id} class=\"${classes}\">\n<div class=\"content\">\n${content}\n</div>${title}\n</div>"
		}
		"audio" => {
			autoplay = flag(attrs, "autoplay")
			controls = if option(attrs, "nocontrols") "" else " controls"
			loop = flag(attrs, "loop")
			title_above = if node.title == "" "" else "<div class=\"title\">${node.title}</div>\n"
			"<div${id} class=\"${classes}\">\n${title_above}<div class=\"content\">\n<audio src=\"${encode(uri(node.target, attrs))}${time_fragment(attrs)}\"${autoplay}${controls}${loop}>\nYour browser does not support the audio tag.\n</audio>\n</div>\n</div>"
		}
		"video" => {
			poster = get(attrs, "poster") ?? (get(attrs, "1") ?? "")
			fullscreen = if option(attrs, "nofullscreen") "" else " allowfullscreen"
			scheme = get(attrs, "asset-uri-scheme") ?? "https"
			scheme_prefix = if scheme == "" "" else "${scheme}:"
			content = if poster == "youtube" {
				segments = node.target.split_on("/")
				first = segments.first() ?? ""
				ids = first.split_on(",")
				target = ids.first() ?? ""
				list = if segments.len() > 1 Ok(Str.join_with(segments.drop_first(1), "/")) else get(attrs, "list")
				playlist = if ids.len() > 1 Ok(Str.join_with(ids.drop_first(1), ",")) else get(attrs, "playlist")
				list_param = match list {
					Ok(value) => "&amp;list=${value}"
					Err(_) => match playlist {
						Ok(value) => "&amp;playlist=${target},${value}"
						Err(_) => if option(attrs, "loop") "&amp;playlist=${target}" else ""
					}
				}
				related = if option(attrs, "related") "1" else "0"
				start = query(attrs, "start", "start")
				end = query(attrs, "end", "end")
				autoplay = if option(attrs, "autoplay") "&amp;autoplay=1" else ""
				loop = if option(attrs, "loop") "&amp;loop=1" else ""
				muted = if option(attrs, "muted") "&amp;mute=1" else ""
				controls = if option(attrs, "nocontrols") "&amp;controls=0" else ""
				fs = if option(attrs, "nofullscreen") "&amp;fs=0" else ""
				modest = if option(attrs, "modest") "&amp;modestbranding=1" else ""
				theme = query(attrs, "theme", "theme")
				lang = query(attrs, "lang", "hl")
				"<iframe${width}${height} src=\"${scheme_prefix}//www.youtube.com/embed/${target}?rel=${related}${start}${end}${autoplay}${loop}${muted}${controls}${list_param}${fs}${modest}${theme}${lang}\" frameborder=\"0\"${fullscreen}></iframe>"
			} else if poster == "vimeo" {
				segments = node.target.split_on("/")
				target = segments.first() ?? ""
				hash = if segments.len() > 1 Ok(Str.join_with(segments.drop_first(1), "/")) else get(attrs, "hash")
				var $params = []
				match hash {
					Ok(value) => {
						$params = $params.append("h=${value}")
					}
					Err(_) => {}
				}
				for opt in ["autoplay", "loop", "muted"] {
					if option(attrs, opt) {
						$params = $params.append("${opt}=1")
					}
				}
				query_string = if $params.is_empty() "" else "?${Str.join_with($params, "&amp;")}"
				start = match get(attrs, "start") {
					Ok(value) => "#at=${value}"
					Err(_) => ""
				}
				"<iframe${width}${height} src=\"${scheme_prefix}//player.vimeo.com/video/${target}${query_string}${start}\" frameborder=\"0\"${fullscreen}></iframe>"
			} else {
				poster_attribute = if poster == "" "" else " poster=\"${encode(uri(poster, attrs))}\""
				preload = match get(attrs, "preload") {
					Ok(value) if value != "" => " preload=\"${encode(value)}\""
					_ => ""
				}
				autoplay = flag(attrs, "autoplay")
				muted = flag(attrs, "muted")
				controls = if option(attrs, "nocontrols") "" else " controls"
				loop = flag(attrs, "loop")
				"<video src=\"${encode(uri(node.target, attrs))}${time_fragment(attrs)}\"${width}${height}${poster_attribute}${autoplay}${muted}${controls}${loop}${preload}>\nYour browser does not support the video tag.\n</video>"
			}
			"<div${id} class=\"${classes}\">${title}\n<div class=\"content\">\n${content}\n</div>\n</div>"
		}
		_ => ""
	}
	body
}

get : MediaAttributes, Str -> Try(Str, [Missing])
get = |attrs, name| match attrs {
	Layered(view) => match first_value(view.before, name) {
		Ok(value) => Ok(value)
		Err(_) => match view.first.get(name) {
			Ok(value) => Ok(value)
			Err(_) => first_value(view.after, name)
		}
	}
	Indexed(first) => first.get(name).map_err(|_| Missing)
	Legacy(source) => first_value(source, name)
}

first_value : List({ name : Str, value : Str }), Str -> Try(Str, [Missing])
first_value = |source, name| {
	for attr in source {
		if attr.name == name {
			return Ok(attr.value)
		}
	}
	Err(Missing)
}

option : MediaAttributes, Str -> Bool
option = |attrs, name| {
	if get(attrs, "${name}-option").is_ok() or get(attrs, "%${name}").is_ok() {
		return True
	}
	for key in ["options", "opts"] {
		for opt in (get(attrs, key) ?? "").split_on(",") {
			if opt.trim() == name {
				return True
			}
		}
	}
	False
}

flag : MediaAttributes, Str -> Str
flag = |attrs, name| if option(attrs, name) " ${name}" else ""

dimension : MediaAttributes, Str, Str -> Str
dimension = |attrs, name, positional| match (
	match get(attrs, name) {
		Ok(value) => Ok(value)
		Err(_) => get(attrs, positional)
	}
) {
	Ok(value) => " ${name}=\"${encode(value)}\""
	Err(_) => ""
}

query : MediaAttributes, Str, Str -> Str
query = |attrs, name, parameter| match get(attrs, name) {
	Ok(value) => "&amp;${parameter}=${value}"
	Err(_) => ""
}

time_fragment : MediaAttributes -> Str
time_fragment = |attrs| {
	start = get(attrs, "start")
	end = get(attrs, "end")
	if start.is_err() and end.is_err() "" else "#t=${start ?? ""}${
		match end {
			Ok(value) => ",${value}"
			Err(_) => ""
		}
	}"
}

link_constraints : MediaAttributes -> Str
link_constraints = |attrs| {
	nofollow = option(attrs, "nofollow")
	match get(attrs, "window") {
		Ok(window) => {
			rel = if window == "_blank" or option(attrs, "noopener") {
				if nofollow " rel=\"nofollow noopener\"" else " rel=\"noopener\""
			} else ""
			" target=\"${encode(window)}\"${rel}"
		}
		Err(_) => if nofollow " rel=\"nofollow\"" else ""
	}
}

uri : Str, MediaAttributes -> Str
uri = |target, attrs| {
	directory = get(attrs, "imagesdir") ?? ""
	path = if directory == "" or target.contains(":") or target.starts_with("/") target else "${directory}/${target}"
	Str.join_with((if directory != "" and !target.contains(":") and !target.starts_with("/") normalize_path(path) else path).split_on(" "), "%20")
}

default_alt : Str -> Str
default_alt = |target| {
	filename = (target.split_on("/").last() ?? target)
	parts = filename.split_on(".")
	stem = if parts.len() > 1 Str.join_with(parts.take_first(parts.len() - 1), ".") else filename
	Str.join_with(Str.join_with(stem.split_on("_"), " ").split_on("-"), " ")
}

encode : Str -> Str
encode = |source| {
	bytes = source.to_utf8()
	var $out = []
	var $pos = 0
	while $pos < bytes.len() {
		b = bytes.get($pos) ?? 0
		if b == '&' {
			var $end = $pos + 1
			while $end < bytes.len() and ((bytes.get($end) ?? 0) == '#' or ((bytes.get($end) ?? 0) >= '0' and (bytes.get($end) ?? 0) <= '9') or ((bytes.get($end) ?? 0) >= 'a' and (bytes.get($end) ?? 0) <= 'z') or ((bytes.get($end) ?? 0) >= 'A' and (bytes.get($end) ?? 0) <= 'Z')) {
				$end = $end + 1
			}
			if $end > $pos + 1 and (bytes.get($end) ?? 0) == ';' {
				$out = $out.append(b)
			} else {
				$out = $out.concat("&amp;".to_utf8())
			}
		} else if b == '<' {
			$out = $out.concat("&lt;".to_utf8())
		}
			else if b == '>' {
				$out = $out.concat("&gt;".to_utf8())
			}
				else if b == '"' {
					$out = $out.concat("&quot;".to_utf8())
				}
					else {
						$out = $out.append(b)
					}
		$pos = $pos + 1
	}
	Str.from_utf8_lossy($out)
}

expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n</div>"
expect AsciiDocMedia.render({ context: "audio", target: "song.mp3", title: "", id: "", attrs: [] }).contains("<audio src=\"song.mp3\" controls>")
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "1", value: "youtube" }, { name: "options", value: "loop,muted" }] }).contains("https://www.youtube.com/embed/abc?rel=0&amp;loop=1&amp;mute=1&amp;playlist=abc")

normalize_path : Str -> Str
normalize_path = |path| {
	scheme_parts = path.split_on("://")
	prefix = if scheme_parts.len() > 1 "${scheme_parts.first() ?? ""}://" else if path.starts_with("/") "/" else ""
	body = if scheme_parts.len() > 1 Str.join_with(scheme_parts.drop_first(1), "://") else path
	var $parts = []
	for part in body.split_on("/") {
		if part == ".." and !$parts.is_empty() and ($parts.last() ?? "") != ".." {
			$parts = $parts.take_first($parts.len() - 1)
		} else if part != "" and part != "." {
			$parts = $parts.append(part)
		}
	}
	"${prefix}${Str.join_with($parts, "/")}"
}

# Live oracle media regression: media-full-image-default
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-alt
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "A cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"A cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-alt-filename
expect AsciiDocMedia.render({ context: "image", target: "some-big_cat.svg", title: "", id: "", attrs: [{ name: "alt", value: "some big cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"some-big_cat.svg\" alt=\"some big cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-alt-multidot
expect AsciiDocMedia.render({ context: "image", target: "some.big_cat.svg", title: "", id: "", attrs: [{ name: "alt", value: "some.big cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"some.big_cat.svg\" alt=\"some.big cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-size
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "Cat" }, { name: "height", value: "80" }, { name: "width", value: "100" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"Cat\" width=\"100\" height=\"80\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-size-percent
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "Cat" }, { name: "height", value: "100%" }, { name: "width", value: "50%" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"Cat\" width=\"50%\" height=\"100%\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-named
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "Cat" }, { name: "height", value: "80" }, { name: "width", value: "100" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"Cat\" width=\"100\" height=\"80\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-quoted
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "Cat, sleeping" }, { name: "height", value: "80" }, { name: "width", value: "100" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"Cat, sleeping\" width=\"100\" height=\"80\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-quote-escape
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "A \"cat\"" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"A &quot;cat&quot;\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-special
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "A <cat> & friend" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"A &lt;cat&gt; &amp; friend\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-role
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "cat" }, { name: "role", value: "red big" }] }) == "<div class=\"imageblock red big\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-id
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "picture", attrs: [{ name: "alt", value: "cat" }] }) == "<div id=\"picture\" class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-title
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "Figure 1. A <strong>cat</strong>", id: "", attrs: [{ name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n<div class=\"title\">Figure 1. A <strong>cat</strong></div>\n</div>"

# Live oracle media regression: media-full-image-caption
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "Illustration 1. A cat", id: "", attrs: [{ name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n<div class=\"title\">Illustration 1. A cat</div>\n</div>"

# Live oracle media regression: media-full-image-custom-caption
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "Picture: A cat", id: "", attrs: [{ name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n<div class=\"title\">Picture: A cat</div>\n</div>"

# Live oracle media regression: media-full-image-dir
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "imagesdir", value: "assets" }, { name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"assets/cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-dir-trailing
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "imagesdir", value: "assets/" }, { name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"assets/cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-dir-parent
expect AsciiDocMedia.render({ context: "image", target: "../cat.png", title: "", id: "", attrs: [{ name: "imagesdir", value: "assets/sub" }, { name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"assets/cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-absolute
expect AsciiDocMedia.render({ context: "image", target: "/cat.png", title: "", id: "", attrs: [{ name: "imagesdir", value: "assets" }, { name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"/cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-remote
expect AsciiDocMedia.render({ context: "image", target: "https://example.org/cat.png", title: "", id: "", attrs: [{ name: "imagesdir", value: "assets" }, { name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"https://example.org/cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-space
expect AsciiDocMedia.render({ context: "image", target: "big cat.png", title: "", id: "", attrs: [{ name: "alt", value: "big cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"big%20cat.png\" alt=\"big cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-align-float
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "align", value: "center" }, { name: "alt", value: "Cat" }, { name: "float", value: "right" }] }) == "<div class=\"imageblock right text-center\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"Cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-link
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "Cat" }, { name: "link", value: "guide.html" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<a class=\"image\" href=\"guide.html\"><img src=\"cat.png\" alt=\"Cat\"></a>\n</div>\n</div>"

# Live oracle media regression: media-full-image-link-window
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "Cat" }, { name: "link", value: "guide.html" }, { name: "window", value: "_blank" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<a class=\"image\" href=\"guide.html\" target=\"_blank\" rel=\"noopener\"><img src=\"cat.png\" alt=\"Cat\"></a>\n</div>\n</div>"

# Live oracle media regression: media-full-image-link-nofollow
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "Cat" }, { name: "link", value: "guide.html" }, { name: "nofollow-option", value: "" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<a class=\"image\" href=\"guide.html\" rel=\"nofollow\"><img src=\"cat.png\" alt=\"Cat\"></a>\n</div>\n</div>"

# Live oracle media regression: media-full-image-svg-secure
expect AsciiDocMedia.render({ context: "image", target: "cat.svg", title: "", id: "", attrs: [{ name: "alt", value: "Cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.svg\" alt=\"Cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-image-data-uri-secure
expect AsciiDocMedia.render({ context: "image", target: "cat.png", title: "", id: "", attrs: [{ name: "alt", value: "cat" }] }) == "<div class=\"imageblock\">\n<div class=\"content\">\n<img src=\"cat.png\" alt=\"cat\">\n</div>\n</div>"

# Live oracle media regression: media-full-video-default
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-size
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "height", value: "360" }, { name: "width", value: "640" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4\" width=\"640\" height=\"360\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-poster
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "height", value: "360" }, { name: "poster", value: "cover.png" }, { name: "width", value: "640" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4\" width=\"640\" height=\"360\" poster=\"cover.png\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-named
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "height", value: "360" }, { name: "poster", value: "cover.png" }, { name: "width", value: "640" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4\" width=\"640\" height=\"360\" poster=\"cover.png\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-directory
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "imagesdir", value: "assets" }, { name: "poster", value: "cover.png" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"assets/clip.mp4\" poster=\"assets/cover.png\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-start-end
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "end", value: "15" }, { name: "start", value: "5" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4#t=5,15\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-end
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "end", value: "15" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4#t=,15\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-start
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "start", value: "5" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4#t=5\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-options
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "autoplay-option", value: "" }, { name: "loop-option", value: "" }, { name: "muted-option", value: "" }, { name: "nocontrols-option", value: "" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4\" autoplay muted loop>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-preload
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "", id: "", attrs: [{ name: "preload", value: "none" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<video src=\"clip.mp4\" controls preload=\"none\">\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-video-metadata
expect AsciiDocMedia.render({ context: "video", target: "clip.mp4", title: "Video <strong>title</strong>", id: "clip", attrs: [{ name: "align", value: "right" }, { name: "float", value: "left" }, { name: "role", value: "red" }] }) == "<div id=\"clip\" class=\"videoblock left text-right red\">\n<div class=\"title\">Video <strong>title</strong></div>\n<div class=\"content\">\n<video src=\"clip.mp4\" controls>\nYour browser does not support the video tag.\n</video>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-default
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "poster", value: "youtube" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=0\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-size
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "height", value: "360" }, { name: "poster", value: "youtube" }, { name: "width", value: "640" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe width=\"640\" height=\"360\" src=\"https://www.youtube.com/embed/abc?rel=0\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-time
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "end", value: "15" }, { name: "poster", value: "youtube" }, { name: "start", value: "5" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=0&amp;start=5&amp;end=15\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-options
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "autoplay-option", value: "" }, { name: "loop-option", value: "" }, { name: "modest-option", value: "" }, { name: "muted-option", value: "" }, { name: "nocontrols-option", value: "" }, { name: "nofullscreen-option", value: "" }, { name: "poster", value: "youtube" }, { name: "related-option", value: "" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=1&amp;autoplay=1&amp;loop=1&amp;mute=1&amp;controls=0&amp;playlist=abc&amp;fs=0&amp;modestbranding=1\" frameborder=\"0\"></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-theme-lang
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "lang", value: "fr" }, { name: "poster", value: "youtube" }, { name: "theme", value: "dark" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=0&amp;theme=dark&amp;hl=fr\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-list
expect AsciiDocMedia.render({ context: "video", target: "abc/list123", title: "", id: "", attrs: [{ name: "poster", value: "youtube" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=0&amp;list=list123\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-playlist
expect AsciiDocMedia.render({ context: "video", target: "abc,def,ghi", title: "", id: "", attrs: [{ name: "poster", value: "youtube" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=0&amp;playlist=abc,def,ghi\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-list-attr
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "list", value: "list123" }, { name: "poster", value: "youtube" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=0&amp;list=list123\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-playlist-attr
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "playlist", value: "def,ghi" }, { name: "poster", value: "youtube" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://www.youtube.com/embed/abc?rel=0&amp;playlist=abc,def,ghi\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-scheme
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "asset-uri-scheme", value: "http" }, { name: "poster", value: "youtube" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"http://www.youtube.com/embed/abc?rel=0\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-youtube-no-scheme
expect AsciiDocMedia.render({ context: "video", target: "abc", title: "", id: "", attrs: [{ name: "asset-uri-scheme", value: "" }, { name: "poster", value: "youtube" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"//www.youtube.com/embed/abc?rel=0\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-vimeo-default
expect AsciiDocMedia.render({ context: "video", target: "123", title: "", id: "", attrs: [{ name: "poster", value: "vimeo" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://player.vimeo.com/video/123\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-vimeo-hash
expect AsciiDocMedia.render({ context: "video", target: "123/xyz", title: "", id: "", attrs: [{ name: "poster", value: "vimeo" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://player.vimeo.com/video/123?h=xyz\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-vimeo-hash-attr
expect AsciiDocMedia.render({ context: "video", target: "123", title: "", id: "", attrs: [{ name: "hash", value: "xyz" }, { name: "poster", value: "vimeo" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://player.vimeo.com/video/123?h=xyz\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-vimeo-time
expect AsciiDocMedia.render({ context: "video", target: "123", title: "", id: "", attrs: [{ name: "poster", value: "vimeo" }, { name: "start", value: "30" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://player.vimeo.com/video/123#at=30\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-vimeo-options
expect AsciiDocMedia.render({ context: "video", target: "123", title: "", id: "", attrs: [{ name: "autoplay-option", value: "" }, { name: "loop-option", value: "" }, { name: "muted-option", value: "" }, { name: "nofullscreen-option", value: "" }, { name: "poster", value: "vimeo" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe src=\"https://player.vimeo.com/video/123?autoplay=1&amp;loop=1&amp;muted=1\" frameborder=\"0\"></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-vimeo-combined
expect AsciiDocMedia.render({ context: "video", target: "123/xyz", title: "", id: "", attrs: [{ name: "autoplay-option", value: "" }, { name: "height", value: "360" }, { name: "loop-option", value: "" }, { name: "muted-option", value: "" }, { name: "poster", value: "vimeo" }, { name: "start", value: "20" }, { name: "width", value: "640" }] }) == "<div class=\"videoblock\">\n<div class=\"content\">\n<iframe width=\"640\" height=\"360\" src=\"https://player.vimeo.com/video/123?h=xyz&amp;autoplay=1&amp;loop=1&amp;muted=1#at=20\" frameborder=\"0\" allowfullscreen></iframe>\n</div>\n</div>"

# Live oracle media regression: media-full-audio-default
expect AsciiDocMedia.render({ context: "audio", target: "song.mp3", title: "", id: "", attrs: [] }) == "<div class=\"audioblock\">\n<div class=\"content\">\n<audio src=\"song.mp3\" controls>\nYour browser does not support the audio tag.\n</audio>\n</div>\n</div>"

# Live oracle media regression: media-full-audio-directory
expect AsciiDocMedia.render({ context: "audio", target: "song.mp3", title: "", id: "", attrs: [{ name: "imagesdir", value: "assets" }] }) == "<div class=\"audioblock\">\n<div class=\"content\">\n<audio src=\"assets/song.mp3\" controls>\nYour browser does not support the audio tag.\n</audio>\n</div>\n</div>"

# Live oracle media regression: media-full-audio-time
expect AsciiDocMedia.render({ context: "audio", target: "song.mp3", title: "", id: "", attrs: [{ name: "end", value: "30" }, { name: "start", value: "10" }] }) == "<div class=\"audioblock\">\n<div class=\"content\">\n<audio src=\"song.mp3#t=10,30\" controls>\nYour browser does not support the audio tag.\n</audio>\n</div>\n</div>"

# Live oracle media regression: media-full-audio-options
expect AsciiDocMedia.render({ context: "audio", target: "song.mp3", title: "", id: "", attrs: [{ name: "autoplay-option", value: "" }, { name: "loop-option", value: "" }, { name: "nocontrols-option", value: "" }] }) == "<div class=\"audioblock\">\n<div class=\"content\">\n<audio src=\"song.mp3\" autoplay loop>\nYour browser does not support the audio tag.\n</audio>\n</div>\n</div>"

# Live oracle media regression: media-full-audio-metadata
expect AsciiDocMedia.render({ context: "audio", target: "song.mp3", title: "Audio <strong>title</strong>", id: "song", attrs: [{ name: "role", value: "red" }] }) == "<div id=\"song\" class=\"audioblock red\">\n<div class=\"title\">Audio <strong>title</strong></div>\n<div class=\"content\">\n<audio src=\"song.mp3\" controls>\nYour browser does not support the audio tag.\n</audio>\n</div>\n</div>"

# Indexed media must retain the legacy first-binding rule, including empty
# presence-based options and named dimensions before positional fallback.
expect {
	attrs = [{ name: "alt", value: "Source alt" }, { name: "alt", value: "Local alt" }, { name: "imagesdir", value: "assets" }, { name: "imagesdir", value: "wrong" }, { name: "width", value: "" }, { name: "2", value: "100" }, { name: "nofollow-option", value: "" }, { name: "link", value: "guide.html" }]
	first = attrs.fold(Dict.empty(), |index, attr| if index.contains(attr.name) index else index.insert(attr.name, attr.value))
	node = { context: "image", target: "cat.png", title: "Figure 1. Cat", id: "cat", attrs }
	indexed = AsciiDocMedia.render_indexed(node, first)
	indexed == AsciiDocMedia.render(node) and indexed.contains("src=\"assets/cat.png\" alt=\"Source alt\" width=\"\"") and indexed.contains("rel=\"nofollow\"")
}

# The same indexed lookup environment covers HTML5 media, both hosted video
# dialects, positional fallback, and option aliases without changing HTML.
expect {
	attrs = [{ name: "role", value: "first" }, { name: "role", value: "last" }, { name: "options", value: "autoplay,loop,muted" }, { name: "opts", value: "nocontrols" }, { name: "start", value: "2" }, { name: "end", value: "9" }, { name: "2", value: "100" }, { name: "3", value: "80" }, { name: "imagesdir", value: "assets" }]
	["audio", "video"].all(
		|context| ["", "youtube", "vimeo"].all(
			|provider| {
				bindings = [{ name: "1", value: provider }].concat(attrs)
				first = bindings.fold(Dict.empty(), |index, attr| if index.contains(attr.name) index else index.insert(attr.name, attr.value))
				node = { context, target: "clip.mp4", title: "Media", id: "clip", attrs: bindings }
				AsciiDocMedia.render_indexed(node, first) == AsciiDocMedia.render(node)
			},
		),
	)
}

# A prepared caller can share a large source index while targets and local
# alt text vary; lookup never depends on the first image's metadata.
expect {
	["one", "two", "three"].all(
		|name| {
			attrs = [{ name: "alt", value: name }, { name: "imagesdir", value: "assets" }]
			first = attrs.fold(Dict.empty(), |index, attr| if index.contains(attr.name) index else index.insert(attr.name, attr.value))
			node = { context: "image", target: "${name}.png", title: "", id: "", attrs }
			AsciiDocMedia.render_indexed(node, first) == AsciiDocMedia.render(node)
		},
	)
}

# A layered prepared caller need not copy a large base dictionary when local
# image metadata differs. Leading/local/base/trailing precedence stays exact.
expect {
	base = [{ name: "alt", value: "Source" }, { name: "imagesdir", value: "assets" }]
	first = base.fold(Dict.empty(), |index, attr| if index.contains(attr.name) index else index.insert(attr.name, attr.value))
	["One", "Two", "Three"].all(
		|alt| {
			before = [{ name: "alt", value: alt }, { name: "width", value: "" }]
			after = [{ name: "alt", value: "Ignored" }, { name: "2", value: "100" }, { name: "link", value: "guide.html" }]
			node = { context: "image", target: "${alt}.png", title: "", id: "", attrs: before.concat(base).concat(after) }
			AsciiDocMedia.render_view(node, { first: AsciiDocSource.from_dict(first), before, after }) == AsciiDocMedia.render(node)
		},
	)
}
