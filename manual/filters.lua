function Div(div)
  if div.classes[1] == "note" then
    if FORMAT == "texinfo" then
      local note_start = pandoc.RawInline("texinfo", "@strong{Note:}")
      local rule = pandoc.HorizontalRule()
      local nl = pandoc.LineBreak()
      return ({note_start} .. div.content .. {rule, nl, nl})
    end
  end
  return div
end
