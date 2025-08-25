-- jira-codeblock-title.lua

function CodeBlock(el)
  -- The #+name: property is stored in el.identifier
   local title = el.identifier
   local lang = el.classes[1]
  if title and title ~= "" then

    -- Construct the appropriate Jira macro
    local macro = "{noformat:title=" .. title .. "}"
    if lang then
      macro = "{code:" .. lang .. "|title=" .. title .. "}"
    end

    -- Construct the full content with the macro
    local new_content = macro .. "\n" .. el.text .. "\n{noformat}"
    if lang then
      new_content = macro .. "\n" .. el.text .. "\n{code}"
    end

    -- Return the content as a raw Jira block
    return pandoc.RawBlock('jira', new_content)
  else
    -- If no identifier/title, return the original element unmodified
    return nil
  end
end

function Subscript(el)
  -- Create a new Str (string) element.
  -- The content of a subscript is a list of inlines.
  -- We join them together to get the plain text.
  local original_text = pandoc.utils.stringify(el.content)

  -- Prepend the underscore and return a single Str element.
  return pandoc.Str('_' .. original_text)
end
