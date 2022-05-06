// Edit this array of apps
var apps = [
  {
    name: 'Strava App',
    path: './strava/',
    image:
      'https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fwww.logolynx.com%2Fimages%2Flogolynx%2Feb%2Feb16b50102a0e2117f4c24c317e1cb60.png&f=1&nofb=1',
    description: 'My own Strava',
  },
];

var app = new Vue({
  el: '#app',
  data: {
    apps: apps,
  },
});
