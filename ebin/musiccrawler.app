%%% -------------------------------------------------------------------
%% Copyright 2011 Ulf Angermann, Martin Huber
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

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
