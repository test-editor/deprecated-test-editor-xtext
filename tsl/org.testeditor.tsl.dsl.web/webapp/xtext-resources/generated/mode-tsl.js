define(["ace/lib/oop", "ace/mode/text", "ace/mode/text_highlight_rules"], function(oop, mText, mTextHighlightRules) {
	var HighlightRules = function() {
    // available tokens: tag, type, keyword, text, string, selection, comment, function, constant.buildin, constant.library, comment, comment.doc, comment.doc.tag, xml-pe, meta.tag, entity.other.attribute-name
		var keywords = "package";
		this.$rules = {
			"start": [
				{token: "keyword", regex: "\\b(?:" + keywords + ")\\b"},
        		{token: [ "text", "type"] , regex: "(\\#)(.*$)" },
				{token: [ "text", "markup.italic.markup.bold" ], regex: "(\\+\\+\\+)([^\\ ])", next: "md-cursive-bold"},
				{token: [ "text", "markup.italic"], regex: "(\\+\\+)([^\\+\\ ])", next: "md-cursive"},
				{token: [ "text", "markup.bold"], regex: "(\\+)([^\\+\\ ])", next: "md-bold"},
        		{token: [ "string" ], regex: "\\\"(\\\\.|[^\\\"])*\\\"" },
        		{token: [ "string" ], regex: "\\\'(\\\\.|[^\\\'])*\\\'" },
        		{token: "comment", regex: "//.*$" },
        		{token: "comment", regex: "/\\*", next: "ml-comment" }
			],
			"md-cursive-bold": [
			    {token: [ "markup.italic.markup.bold", "text" ], regex: "([^ ])(\\+\\+\\+)", next: "start" },
          		{defaultToken: "markup.italic.markup.bold" }
			],
			"md-cursive": [
			    {token: [ "markup.italic", "text" ], regex: "([^ ])(\\+\\+)", next: "start" },
          		{defaultToken: "markup.italic" }
			],
			"md-bold": [
			    {token: [ "markup.bold", "text" ], regex: "([^ ])\\+", next: "start" },
          		{defaultToken: "markup.bold" }
			],
      		"ml-comment": [
        		{token: "comment", regex: "\\*/", next: "start" },
        		{defaultToken: "comment" }
      		]
		};
	};
	oop.inherits(HighlightRules, mTextHighlightRules.TextHighlightRules);

	var Mode = function() {
		this.HighlightRules = HighlightRules;
	};
	oop.inherits(Mode, mText.Mode);
	Mode.prototype.$id = "xtext/tsl";
	Mode.prototype.getCompletions = function(state, session, pos, prefix) {
		return [];
	}

	return {
		Mode: Mode
	};
});

