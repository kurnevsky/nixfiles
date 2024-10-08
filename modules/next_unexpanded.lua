local mp = require 'mp'

function next_unexpanded(step, last)
  local count = tonumber(mp.get_property("playlist-count"))
  local pos = tonumber(mp.get_property("playlist-pos"))
  if count == 0 or pos == -1 then
     return
  end

  local path = mp.get_property("playlist/" .. pos .. "/playlist-path")

  if path == nil then
    pos = pos + step
  else
    while pos >= 0 and pos < count do
      pos = pos + step
      if mp.get_property("playlist/" .. pos .. "/playlist-path") ~= path then
        break
      end
    end
  end

  if pos >= 0 and pos < count then
    if last then
      path = mp.get_property("playlist/" .. pos .. "/playlist-path")
      pos = pos + step
      while pos >= 0 and pos < count and mp.get_property("playlist/" .. pos .. "/playlist-path") == path do
        pos = pos + step
      end
      pos = pos - step
    end
    mp.commandv("playlist-play-index", pos)
  end
end

mp.add_key_binding("Ctrl+>", "next-unexpanded", function()
    next_unexpanded(1, false)
  end
)

mp.add_key_binding("Ctrl+<", "prev-unexpanded", function()
    next_unexpanded(-1, true)
  end
)
