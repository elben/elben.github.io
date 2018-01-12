var trie = null;

$(function () {
  // Global vars for easier debugging
  clustering = Clustering();
  mycanvas = Canvas("canvas");

  var blanks = /^\W*$/;

  function cluster() {
    var numClusters = parseInt($("input.numClusters").val());
    clustering.cluster(numClusters);
    mycanvas.refreshData(clustering);
  }

  function clear() {
    $(".textarea").val("");
    save();
  }

  function save() {
    var text = $(".textarea").val();
    var strings = text.split(/\n+/);
    var data = [];
    for (var i in strings) {
      var string = strings[i].toLowerCase();
      if (!blanks.exec(string)) {
        // Not blank
        data.push(string.split(/,*\W/));
      }
    }

    // Reset K-means
    self.clustering = Clustering();
    self.clustering.importData(data);
    mycanvas.refreshData(self.clustering);

    $("#modified").hide();
    $("#saved").show();
  }

  $(".autocomplete").keyup(function (e) {
    $(".words").empty();

    var val = $(this).val();

    if (val && val.length > 0) {
      var results = trie.search($(this).val());
      for (var i in results) {
        var result = results[i];
        $(".words").append("<li>" + result + "</li>");
      }
    }
  });

  $(".contains").keyup(function (e) {
    var val = $(this).val();

    if (val && val.length > 0) {
      if(trie.includes($(this).val())) {
        $("#contains-result").html("true");
      } else {
        $("#contains-result").html("false");
      }
    } else {
      $("#contains-result").empty();
    }
  });

  $(".textarea").keyup(function () {
    $("#modified").show();
    $("#saved").hide();
  });

  save();
  $(".save").click(save);
  $(".clear").click(clear);
  $(".cluster").click(cluster);
});

