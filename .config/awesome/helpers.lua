-- Standard libs
local awful   = require("awful")
local naughty = require("naughty")

-- Helper functions

function run(cmd)
   awful.spawn.with_shell(cmd)
end

function run_once(prg,arg)
   if not prg then
      do return nil end
   end
   if not arg then
      arg = ""
   else
      arg = " " .. arg
   end
   awful.spawn.with_shell("pgrep -u $USER -x " .. prg .. " || (" .. prg .. arg .. " & )")
end

function run_w_tag(i, prg)
   local screen = mouse.screen
   awful.spawn(prg)
   if tags[screen][i] then
      awful.tag.viewonly(tags[screen][i])
   end
end

function audioctl_alsa(cmd)
   local fh = io.popen("amixer set Master " .. cmd)
   local res = ""
   for i in fh:lines() do
      res = i
   end
   local s, e, vol = string.find(res, '%[(%d+)%%%]')
   if string.find(res, '%[off%]') then
      vol = "muted"
   else
      vol = vol .. "%"
   end
   io.close(fh)
   naughty.notify({ text="Master " .. vol, timeout=0.5 })
end

function grep_output(cmd,str)
   local fh = io.popen(cmd)
   local res = ""
   for i in fh:lines() do
      res = i
   end
   io.close(fh)
   local s, e, out = string.find(res, str)
   return out
end

function audioctl(cmd)
   -- Get current sink
   local sink = grep_output("pacmd list-sinks | grep 'index: '",'(%d+)')
   -- Set volume or mute
   if ( cmd == "toggle" ) then
      local fh = io.popen("pactl set-sink-mute " .. sink .. " toggle")
      io.close(fh)
   else
      local fh = io.popen("pactl set-sink-volume " .. sink .. " " .. cmd)
      io.close(fh)
   end
   -- Get volume
   local vol = grep_output("pacmd list-sinks | grep 'volume: front-left:'", '(%d+)%%')
   if ( tonumber(vol) > 100 ) then
      local fh = io.popen("pactl set-sink-volume " .. sink .. " 100%")
      io.close(fh)
      vol = "100"
   end
   if ( tonumber(vol) < 0 ) then
      local fh = io.popen("pactl set-sink-volume " .. sink .. " 0%")
      io.close(fh)
      vol = "0"
   end
   -- Find if muted
   local mute = grep_output("pacmd list-sinks | grep 'muted:'", 'muted: (.+)')
   if ( mute == "yes" ) then
      vol = "muted"
   else
      vol = vol .. "%"
   end
   naughty.notify({ text="Master " .. vol, timeout=0.5 })
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

   local current = grep_output("xbacklight -get",'(%S+)')
   naughty.notify({ text="Brightness " .. current, timeout=0.5 })
end


function finalize()
   run_once("mate-settings-daemon")
   run_once("gnome-keyring-daemon","--start --components=gpg,pkcs11,secrets")
   run_once("gnome-screensaver")
   run_once("nm-applet")
   run_once("redshift")
   run_once("dropbox","start")
   run_once("emacs","--daemon")
   run_once("xmodmap","~/.Xmodmap")
end
