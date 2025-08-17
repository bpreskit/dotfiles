function Subscript(el)
  -- Create a new Str (string) element.
  -- The content of a subscript is a list of inlines.
  -- We join them together to get the plain text.
  local original_text = pandoc.utils.stringify(el.content)

  -- Prepend the underscore and return a single Str element.
  return pandoc.Str('_' .. original_text)
end
