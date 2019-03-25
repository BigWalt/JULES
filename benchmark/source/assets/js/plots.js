/***************************************************************
 * Set up the collapsible sections
 ***************************************************************/
d3.selectAll('section:not(.skipped) > header').on('click', function() {
    var parent = d3.select(this.parentNode);
    parent.classed("collapsed", !parent.classed("collapsed"));
});


/***************************************************************
 * Find all the FLUXNET plots and initialise them
 ***************************************************************/
 
var MONTH_NAMES = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
 
d3.selectAll(".fluxnet-plot").each(function() {
    var chart = nv.models.lineChart()
        .margin({ left: 80 })
        .useInteractiveGuideline(true)
        .transitionDuration(350)
        .showLegend(true)
        .showXAxis(true)
        .showYAxis(true);
   
    chart.xAxis
        .axisLabel("Month")
        .tickFormat(function(d) { return MONTH_NAMES[d]; });
       
    chart.yAxis.axisLabel(d3.select(this).attr("data-ylab")).tickFormat(d3.format(',.2f'));
   
    nv.utils.windowResize(chart.update);
     
    d3.json(d3.select(this).attr("data-file"), function(error, data) {
        d3.select(this).append("svg").datum(data).call(chart);
    }.bind(this));
});


/***************************************************************
 * Find all the lai plots and initialise them
 ***************************************************************/
 
d3.selectAll(".lai-plot").each(function() {
    var chart = nv.models.multiChart()
        .margin({top: 30, right: 80, bottom: 50, left: 80})
        .showLegend(true);
        
    chart.xAxis
        .axisLabel("Month")
        .tickFormat(function(d) { return MONTH_NAMES[d]; });
       
    chart.yAxis1.axisLabel("Observed NDVI").tickFormat(d3.format(',.2f'));
    chart.yAxis2.axisLabel("Modelled LAI").tickFormat(d3.format(',.2f'));
   
    nv.utils.windowResize(chart.update);
     
    d3.json(d3.select(this).attr("data-file"), function(error, data) {
        d3.select(this).append("svg").datum(data).call(chart);
    }.bind(this));
});


/***************************************************************
 * Find all the frac plots and initialise them
 ***************************************************************/
 
var FT_NAMES = ['BL', 'NL', 'C3', 'C4', 'SH', 'BS']
 
d3.selectAll(".frac-plot").each(function() {
    var chart = nv.models.multiBarChart()
      .margin({ left: 80, bottom : 30 })
      .reduceXTicks(false)
      .rotateLabels(0)
      .showControls(false)
      .groupSpacing(0.2);
        
    chart.xAxis.tickFormat(function(d) { return FT_NAMES[d]; });
       
    chart.yAxis.axisLabel("Fraction").tickFormat(d3.format(',.3f'));
   
    nv.utils.windowResize(chart.update);
     
    d3.json(d3.select(this).attr("data-file"), function(error, data) {
        d3.select(this).append("svg").datum(data).call(chart);
    }.bind(this));
});


/*****************************************************************
 * Find all the closure maps and initialise them
 *****************************************************************/
var MAP_WIDTH = 850;

var MAP_COLORS = d3.scale.threshold()
    .domain([-1e30, 0, 1e-4, 1e-2])
    .range(["#000", "#fff", "#00F500", "#FAFA00", "#F20000"]);

d3.selectAll(".closure-map").each(function() {
    var figure = d3.select(this);

    d3.json(figure.attr("data-file"), function(error, data) {
        var dx = data[0].length, dy = data.length;
        
        // Fix the aspect ratio based on dx and dy
        var width = MAP_WIDTH,
            height = width * dy / dx;

        // Append a canvas to contain the image
        figure.append("canvas")
            .attr("width", dx)
            .attr("height", dy)
            .style("width", width + "px")
            .style("height", height + "px")
            .call(drawImage);
   
        // Compute the pixel colors; scaled by CSS.
        function drawImage(canvas) {
            var context = canvas.node().getContext("2d"),
                image = context.createImageData(dx, dy);
   
            for (var y = 0, p = -1; y < dy; ++y) {
                for (var x = 0; x < dx; ++x) {
                    var c = d3.rgb(MAP_COLORS(data[dy-y-1][x]));
                    image.data[++p] = c.r;
                    image.data[++p] = c.g;
                    image.data[++p] = c.b;
                    image.data[++p] = 255;
                }
            }
   
            context.putImageData(image, 0, 0);
        }
    });
});