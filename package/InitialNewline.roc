## One-token HTML initial-line-feed state, created from an element's text behavior.
InitialNewline :: [Pending, Preserve].{
	is_eq : _
	to_hash : _
	Token : [Characters(Str), Comment, Other]

	from_behavior : [DropInitialLf, PreserveInitialLf] -> InitialNewline
	from_behavior = |behavior| InitialNewline.(if (behavior == DropInitialLf) Pending else Preserve)

	is_pending : InitialNewline -> Bool
	is_pending = |InitialNewline.(state)| state == Pending

	## Empty character bindings preserve pending state; any other token consumes it.
	step : InitialNewline, Token -> { state : InitialNewline, token : Token, action : [Skipped, Preserved] }
	step = |current, token| match token {
		Characters(text) if text.is_empty() => { state: current, token, action: Preserved }
		Characters(text) if current.is_pending() and text.starts_with("\n") => {
			state: from_behavior(PreserveInitialLf),
			token: Characters(text.drop_prefix("\n")),
			action: Skipped,
		}
		_ => { state: from_behavior(PreserveInitialLf), token, action: Preserved }
	}
}
