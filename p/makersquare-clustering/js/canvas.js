var Canvas = function(id) {
  var self = this;

  this.canvas = document.getElementById(id);
  this.context = this.canvas.getContext("2d");

  // Is the mouse currently clicked down, thus we are in "paint" mode?
  this.painting = false;

  this.mousebutton = function (e) {
    if (e.type == "mousedown") {
      self.painting = true
    } else {
      self.painting = false
    }
  };

  this.mousemove = function(e) {
    if (self.painting) {
      var pos = getCursorPosition(e);

      var numPoints = self.randomInt(5, 20);

      for (var i = numPoints; i > 0; --i) {
        var jitterX = pos.x + self.randomInt(-10, 10);
        var jitterY = pos.y + self.randomInt(-10, 10);
        self.data.push([jitterX, jitterY]);
        self.drawPointWithColor(jitterX, jitterY, "#fff");
      }
    }
  };

  // Random number, min (inclusive) to max (exclusive).
  this.randomInt = function(min, max) {
    var diff = max - min;
    return min + Math.floor(Math.random() * diff);
  };

  this.refreshData = function(clustering) {
    // Clear canvas
    this.canvas.width = this.canvas.width;

    if (clustering.clusters.length == 0) {
      // Never ran a "found cluster", so just display points.
      for (var i in clustering.data) {
        var datum = clustering.data[i];
        this.drawPointWithColor(datum[0], datum[1], "#fff");
      }
    } else {
      for (var ci in clustering.clusters) {
        var cluster = clustering.clusters[ci];
        for (var pi in cluster) {
          var datum = cluster[pi];
          this.drawPointWithColorIndex(datum[0], datum[1], ci);
        }
      }
    }
  }

  this.drawPointWithColor = function (x, y, color, size) {
    var oldFillStyle = this.context.fillStyle;
    this.context.fillStyle = color;
    this.drawPoint(x, y, size);
    this.context.fillStyle = oldFillStyle;
  }

  this.drawPointWithColorIndex = function (x, y, colorIndex, size) {
    this.drawPointWithColor(x, y, this.getColor(colorIndex), size);
  };

  this.drawPoint = function (x, y, size) {
    if (size === undefined) {
      size = 1;
    }

    this.context.fillRect(x, y, size, size);
  };

  this.clusterColors = ["#00ead2", "#6055e5", "#3eaee5", "#9a2de4", "#e200e3", "#f41dac", "#f33560", "#ee824b", "#e2d44b", "#9aec51", "#00ef55", "#00ee8a"];

  this.getColor = function (idx) {
    return this.clusterColors[idx % this.clusterColors.length];
  };

  this.getCursorPosition = function (e) {
    var x;
    var y;

    if (e.pageX != undefined && e.pageY != undefined) {
      x = e.pageX;
      y = e.pageY
    } else {
      x = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
      y = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
    }

    x -= this.canvas.offsetLeft;
    y -= this.canvas.offsetTop;

    return {x: x, y: y};
  };

  this.canvas.addEventListener("mousedown", this.mousebutton, false);
  this.canvas.addEventListener("mouseup", this.mousebutton, false);
  this.canvas.addEventListener("mousemove", this.mousemove, false);

  return this;
}
