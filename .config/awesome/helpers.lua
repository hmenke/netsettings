-- Standard libs
local awful   = require("awful")
local naughty = require("naughty")

-- Helper functions

function run_once(prg,arg)
   if not prg then
      return
   end
   if not arg then
      arg = ""
   else
      arg = " " .. arg
   end
   awful.spawn.with_shell("pgrep -u $USER -x " .. prg .. " || (" .. prg .. arg .. " & )")
end

function grep_output(cmd,str)
   local fh = io.popen(cmd)
   local out = {}
   for line in fh:lines() do
      local _, _, res = string.find(line, str)
      out[#out+1] = res
   end
   io.close(fh)
   return out
end

function audioctl(cmd)
   -- Get current sink
   local sinks = grep_output("pactl list sinks | grep Sink",'(%d+)')
   -- Set volume or mute
   for _,sink in ipairs(sinks) do
       if ( cmd == "toggle" ) then
           local fh = io.popen("pactl set-sink-mute " .. sink .. " toggle")
           io.close(fh)
       else
           local fh = io.popen("pactl set-sink-volume " .. sink .. " " .. cmd)
           io.close(fh)
       end
   end
   -- Get volume
   local vols = grep_output("pacmd list-sinks | grep 'volume: front-left:'", '(%d+)%%')
   for i,vol in ipairs(vols) do
       if ( tonumber(vol) > 100 ) then
           local fh = io.popen("pactl set-sink-volume " .. sinks[i] .. " 100%")
           io.close(fh)
           vol = "100"
       elseif ( tonumber(vol) < 0 ) then
           local fh = io.popen("pactl set-sink-volume " .. sinks[i] .. " 0%")
           io.close(fh)
           vol = "0"
       end
   end
   -- Find if muted
   local mutes = grep_output("pactl list sinks | grep Mute:", 'Mute: (.+)')
   for i,mute in ipairs(mutes) do
       if ( mute == "yes" ) then
           vols[i] = "muted"
       else
           vols[i] = vols[i] .. "%"
       end
       naughty.notify({ text="Sink " .. sinks[i] .. " " .. vols[i], timeout=0.5 })
   end
end

function brightness(cmd)
   local op = cmd:sub(1,1)
   if op == "+" or op == "-" then
      local fh = io.popen("xbacklight -inc " .. cmd)
      io.close(fh)
   else
      local fh = io.popen("xbacklight -set " .. cmd)
      io.close(fh)
   end

   local current = grep_output("xbacklight -get",'(%S+)')[1]
   naughty.notify({ text="Brightness " .. current, timeout=0.5 })
end

function notify_systemd_errors()
   local cmd = "systemctl --failed --all --no-legend --no-pager"
   local fh = io.popen(cmd)
   lines = {}
   for line in fh:lines() do
       lines[#lines+1] = line
   end
   local failures = table.concat(lines,"\n")
   if failures ~= "" then
      naughty.notify({ title="SystemD failures",
                       text=table.concat(lines,"\n"),
                       timeout=10.0, fg="white", bg="red" })
   end
   fh:close()
end

function finalize()
   run_once("mate-settings-daemon")
   run_once("xscreensaver","-nosplash")
   run_once("nm-applet")
   run_once("redshift")
   run_once("dropbox","start")
   run_once("xmodmap","~/.Xmodmap")
   notify_systemd_errors()
end
