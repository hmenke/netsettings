unlet b:current_syntax
syn include @luaSyn syntax/lua.vim

syn region luaZone matchgroup=texBeginEnd
                   \ start='\\begin{luacode\*\{,1}}'
                   \ end='\\end{luacode\*\{,1}}' keepend
                   \ contains=@luaSyn,@NoSpell

let b:current_syntax = 'tex'
