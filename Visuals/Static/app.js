/* Static municipality map.
 *
 * All values are precomputed and embedded in this file, so the controls work
 * without a server. RATE and TEMP are flat integer arrays ordered by
 * municipality, then by TIMELINE position: value(i, t) = array[i * NW + t].
 * Rates are stored as rate * 1e5, temperatures as degrees C * 10, and -999
 * marks a missing value.
 */
(function () {
  "use strict";

  var SENT = -999;
  var NW = TIMELINE.length;
  var N = IDS.length;

  // ---- Ring winding -----------------------------------------------------
  // The GeoJSON follows RFC 7946, but the d3-geo renderer used by plotly
  // expects clockwise exterior rings and counter-clockwise holes. With the
  // RFC winding, some municipalities fill the whole map frame.
  function ringArea(ring) {
    var a = 0;
    for (var i = 0, j = ring.length - 1; i < ring.length; j = i++) {
      a += ring[j][0] * ring[i][1] - ring[i][0] * ring[j][1];
    }
    return a / 2;
  }

  function rewindRing(ring, exterior) {
    var area = ringArea(ring);
    var wrong = exterior ? area > 0 : area < 0;
    return wrong ? ring.slice().reverse() : ring;
  }

  function rewindPolygon(poly) {
    return poly.map(function (ring, i) {
      return rewindRing(ring, i === 0);
    });
  }

  GEO.features.forEach(function (f) {
    var g = f.geometry;
    if (!g || !g.coordinates) return;
    g.coordinates = g.type === "Polygon"
      ? rewindPolygon(g.coordinates)
      : g.coordinates.map(rewindPolygon);
  });

  // ---- Controls ---------------------------------------------------------
  var yearSel = document.getElementById("year");
  var weekSel = document.getElementById("week");
  var mapDiv = document.getElementById("map");

  var years = [];
  TIMELINE.forEach(function (t) {
    if (years.indexOf(t[0]) < 0) years.push(t[0]);
  });
  years.sort(function (a, b) { return a - b; });

  years.forEach(function (y) {
    var o = document.createElement("option");
    o.value = String(y);
    o.textContent = String(y);
    yearSel.appendChild(o);
  });
  yearSel.value = String(years[years.length - 1]);

  function weeksOf(year) {
    return TIMELINE
      .filter(function (t) { return t[0] === year; })
      .map(function (t) { return t[1]; });
  }

  function fillWeeks() {
    var weeks = weeksOf(Number(yearSel.value));
    weekSel.innerHTML = "";
    weeks.forEach(function (w) {
      var o = document.createElement("option");
      o.value = String(w);
      o.textContent = String(w);
      weekSel.appendChild(o);
    });
    weekSel.value = String(weeks[0]);
  }

  function timelineIndex(year, week) {
    for (var i = 0; i < NW; i++) {
      if (TIMELINE[i][0] === year && TIMELINE[i][1] === week) return i;
    }
    return -1;
  }

  // ---- Traces -----------------------------------------------------------
  var GREY = {
    type: "choropleth",
    geojson: GEO,
    locations: IDS,
    z: IDS.map(function () { return 0; }),
    featureidkey: "properties.iso",
    colorscale: [[0, "#e9e9e9"], [1, "#e9e9e9"]],
    showscale: false,
    hoverinfo: "skip",
    marker: { line: { color: "#ffffff", width: 0.3 } }
  };

  function values(metric, ti) {
    var src = metric === "rate" ? RATE : TEMP;
    var out = new Array(N);
    for (var i = 0; i < N; i++) {
      var v = src[i * NW + ti];
      out[i] = v === SENT
        ? null
        : (metric === "rate" ? v / 1e5 : v / 10);
    }
    return out;
  }

  function draw() {
    var year = Number(yearSel.value);
    var week = Number(weekSel.value);
    var ti = timelineIndex(year, week);
    var isRate =
      document.querySelector('input[name="metric"]:checked').value === "rate";

    var trace = {
      type: "choropleth",
      geojson: GEO,
      locations: IDS,
      z: values(isRate ? "rate" : "temp", ti),
      customdata: NAMES,
      featureidkey: "properties.iso",
      colorscale: [[0, "#2166ac"], [0.5, "#f7f7f7"], [1, "#b2182b"]],
      zmin: isRate ? 0 : CFG.temp_min,
      zmax: isRate ? CFG.rate_zmax : CFG.temp_max,
      colorbar: {
        title: isRate ? "Rate" : "degrees C",
        tickformat: isRate ? ".4f" : ".1f",
        len: 0.65,
        thickness: 16
      },
      marker: { line: { color: "#ffffff", width: 0.3 } },
      hovertemplate:
        "<b>%{customdata}</b><br>" +
        "Year: " + year + "<br>" +
        "Week: " + week + "<br>" +
        (isRate ? "Age-standardised rate: %{z:.4f}" : "t2m_mean: %{z:.1f} &deg;C") +
        "<extra></extra>"
    };

    var layout = {
      title: {
        text:
          "<b>Austria, " + year + ", week " + week + "</b><br><sup>" +
          (isRate
            ? "Weekly Age-Standardised Hospitalisation Rate"
            : "Mean weekly temperature (week_t2m_mean)") +
          "</sup>",
        x: 0.02,
        xanchor: "left"
      },
      geo: {
        visible: false,
        resolution: 50,
        projection: { type: "mercator" },
        lonaxis: { range: CFG.lon_range },
        lataxis: { range: CFG.lat_range },
        bgcolor: "#ffffff"
      },
      margin: { l: 0, r: 0, t: 55, b: 0 },
      paper_bgcolor: "#ffffff",
      showlegend: false
    };

    Plotly.react(mapDiv, [GREY, trace], layout, {
      displayModeBar: false,
      responsive: true
    });
  }

  yearSel.addEventListener("change", function () {
    fillWeeks();
    draw();
  });
  weekSel.addEventListener("change", draw);
  Array.prototype.forEach.call(
    document.querySelectorAll('input[name="metric"]'),
    function (el) { el.addEventListener("change", draw); }
  );

  fillWeeks();
  draw();
})();
