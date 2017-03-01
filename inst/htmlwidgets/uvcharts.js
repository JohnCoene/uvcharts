HTMLWidgets.widget({

  name: 'uvcharts',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        var config = x.config;

        config.meta.position = "#" + el.id;

        var chart = uv.chart(x.type, x.graph, config);

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
