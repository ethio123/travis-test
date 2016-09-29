(function(chartApp) {
  chartApp.AppComponent = ng.core.Component({
    selector: 'angular-chart',
    templateUrl: '/assets/chart.html',
    providers: [ng.http.HTTP_PROVIDERS]
  }).Class({
    constructor: [ng.http.Http, function(http){
      var chartComponent = this;

      http.get('/assets/config.json')
        .map(function(data){ return data.json() })
        .subscribe(function(json){ chartComponent.initChart(json) });
    }],
    initChart: function(config){
      FusionCharts.ready(function(){
        new FusionCharts(config).render();
      });
    }
  });

  document.addEventListener('DOMContentLoaded', function() {
    ng.platform.browser.bootstrap(chartApp.AppComponent);
  });
})(window.chartApp || (window.chartApp = {}));