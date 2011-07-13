{application, musiccrawler,
 [
  {description, "Das ist der Musik Crawler"},
  {vsn, "stage 1"},
  {modules, [
  				mc_sup,
  				mc_config,
  				mc_incy
  			]},
  {registered, [mc_sup]},
  {applications, [kernel,stdlib]},
  {mod, { mc_app, []}},
  {env, []}
 ]}.
