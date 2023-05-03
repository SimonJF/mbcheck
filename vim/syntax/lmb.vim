syn keyword lmbTodo contained TODO FIXME XXX NOTE
syn match lmbComment "#.*$" contains=lmbTodo

"----------------------------------------------------------------
" lmb files (based off http://vim.wikia.com/wiki/Creating_your_own_syntax_files)
" Also some regexes pinched from the Idris vim mode.
"----------------------------------------------------------------

" Regular int like number with - + or nothing in front
syn match lmbNumber '\d\+' display
syn match lmbNumber '[-+]\d\+' display

" Floating point number with decimal no E or e (+,-)
syn match lmbNumber '\d\+\.\d*' display
syn match lmbNumber '[-+]\d\+\.\d*' display

" Types and identifiers
syn match lmbType '[A-Z][A-Za-z0-9_]*' display
syn match lmbIdentifier "[a-z][a-zA-z0-9_]*\('\)*"
syn match lmbComment "?" contains=lmbSymbol
syn match lmbComment "!" contains=lmbSymbol
syn region lmbString start='"' end='"'

" Keywords
syn keyword lmbKeywords def free fail receive from let in spawn
syn keyword lmbKeywords guard new interface fun linfun

" Library functions
syn keyword linksLibWord print intToString

" Conditionals
syn keyword lmbConditional if else true false

highlight def link lmbComment Comment
highlight def link lmbNumber Number
highlight def link lmbString String
highlight def link lmbConditional Conditional
highlight def link lmbTodo Todo
highlight def link lmbKeywords Keyword
highlight def link lmbIdentifier Normal
highlight def link linksLibWord Identifier
highlight def link lmbType Type
highlight def link lmbSymbol SpecialChar
