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
   -- Set volume or mute
   if ( cmd == "toggle" ) then
      local fh = io.popen("pactl set-sink-mute @DEFAULT_SINK@ toggle")
      io.close(fh)
   else
      local fh = io.popen("pactl set-sink-volume @DEFAULT_SINK@ " .. cmd)
      io.close(fh)
   end
   -- Find if muted
   local mute = grep_output("pulsemixer --get-mute", '(%d)')[1]
   if ( mute == "1" ) then
      naughty.notify({ text="muted", timeout=0.5 })
   else
      local vol = grep_output("pulsemixer --get-volume", '(%d+)')[1]
      naughty.notify({ text="Volume " .. vol .. "%", timeout=0.5 })
   end
end

function micctl(cmd)
   if ( cmd == "toggle" ) then
      local fh = io.popen("pactl set-source-mute alsa_input.pci-0000_00_1b.0.analog-stereo toggle")
      io.close(fh)
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
   run_once("mate-power-manager")
   run_once("xscreensaver","-nosplash")
   run_once("nm-applet")
   run_once("redshift")
   run_once("dropbox","start")
   run_once("xmodmap","~/.Xmodmap")
   run_once("xrdb","~/.Xresources")
   notify_systemd_errors()
end
