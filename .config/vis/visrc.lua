require('vis')

vis.events.subscribe(vis.events.INIT, function()
	require('mappings')
end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)

end)
