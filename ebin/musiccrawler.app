{application, musiccrawler,
 [
  {description, "Das ist der Musik Crawler"},
  {vsn, "stage 1"},
  {modules, [
  				mc_app,
  				mc_sup,
  				mc_controller,
  				mc_config,
  				mc_icy
  			]},
  {registered, [mc_sup, mc_controller]},
  {applications, [kernel,stdlib]},
  {mod, { mc_app, []}}
]}.
