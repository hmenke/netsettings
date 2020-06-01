unlet b:current_syntax
syn include @luaSyn syntax/lua.vim

syn region luaZone
   \ start='\\begin{luacode\*\?}'rs=s
   \ end='\\end{luacode\*\?}'re=e
   \ keepend
   \ transparent
   \ contains=texBeginEnd,@luaSyn,@NoSpell

syn match texStatement '\\\(directlua\|luaexec\|luadirect\)' nextgroup=texZoneLuaArg
syn region texZoneLuaArg matchgroup=Delimiter
   \ start='{'
   \ end='}'
   \ contained
   \ contains=@luaSyn,@NoSpell

let b:current_syntax = 'tex'
