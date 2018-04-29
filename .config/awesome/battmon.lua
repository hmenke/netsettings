---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2012, Axel Arnold <arnolda _at_ icp.uni-stuttgart.de>
--  * (c) 2018, Henri Menke
---------------------------------------------------

local tonumber = tonumber
local timer = timer
local pairs = pairs
local print = print
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local io = require("io")
local string = require("string")
local math = require("math")

local battmon = wibox.widget{ widget=wibox.widget.textbox }

local function battmon_update ()
   local fd = io.popen("acpi -b", "r") --list present batteries
   local line = fd:read()
   fd:close()
   if line then
      local battery_load = tonumber(string.match(line, " (%d*)%%"))
      local time_rem = string.match(line, "(%d+:%d+):%d+")
      local color = "#DCDCCC"

      if string.match(line, "Discharging") ~= "Discharging" then
	 color = "green"
      else
         if battery_load < 10 then
	    color = "red"
	    if battery_load < 5 then
               naughty.notify{ text="Battery level is critical! " .. battery_load .. "%",
                               timeout=10.0, fg="white", bg="red" }
	    end
         elseif battery_load < 20 then
	    color = "orange"
         end
      end
      local txt = [[<span color="]] .. color .. [["> ]] .. math.ceil(battery_load) .. [[% ]]
      if time_rem then
         txt = txt .. time_rem .. " "
      end
      txt = txt .. [[</span>]]
      battmon.markup = txt
   end
end

battmon_update()
battmon_timer=gears.timer{timeout=10}
battmon_timer:connect_signal("timeout", battmon_update)
battmon_timer:start()

return { battmon = battmon }

