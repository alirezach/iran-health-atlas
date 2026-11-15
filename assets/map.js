function (el, x, data) {
  // Interactive map controller: layers, province + threshold filters, legend.
  var map = this;
  var base = data.base;
  var cats = data.categories;
  var byId = function (id) { return document.getElementById(id); };
  var layerSel = byId('atlas-layer');
  var provSel = byId('atlas-province');
  var thrSel = byId('atlas-threshold');
  var hospToggle = byId('atlas-hospitals');
  var legend = byId('atlas-legend');
  var stats = byId('atlas-stats');
  if (!layerSel || !legend) return;

  map.createPane('surface');
  map.getPane('surface').style.zIndex = 350;
  map.createPane('outlines');
  map.getPane('outlines').style.zIndex = 450;
  map.createPane('sites');
  map.getPane('sites').style.zIndex = 460;

  var ov = data.overlays;
  var rasters = {
    travel: L.imageOverlay(base + ov.travel.file, ov.travel.bounds, {pane: 'surface', opacity: 0.85}),
    population: L.imageOverlay(base + ov.population.file, ov.population.bounds, {pane: 'surface', opacity: 0.85})
  };
  var fullBounds = L.latLngBounds(ov.travel.bounds);
  var catLayer = null, provLayer = null, hospLayer = null, provIndex = {};

  function esc(s) {
    var map = {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'};
    return String(s === null || s === undefined ? '' : s).replace(/[&<>"']/g, function (c) { return map[c]; });
  }
  function pct(v) { return (v === null || v === undefined) ? '-' : Math.round(100 * v) + '%'; }
  function num(v) { return (v === null || v === undefined) ? '-' : Math.round(v).toLocaleString('en-US'); }
  function threshold() { return +thrSel.value; }
  function province() { return provSel.value; }

  function catStyle(f) {
    var p = f.properties;
    var sel = province();
    var inProv = sel === 'all' || p.province === sel;
    var beyond = p.lower >= threshold();
    return {stroke: false, fillColor: p.color, fillOpacity: inProv ? (beyond ? 0.9 : 0.28) : 0.06};
  }
  function provStyle(f) {
    var on = province() === f.properties.province;
    return {color: on ? '#13204A' : '#4B5468', weight: on ? 2.5 : 0.6, opacity: on ? 1 : 0.55, fill: true, fillOpacity: 0};
  }

  function renderLegend() {
    var v = layerSel.value;
    var html = '';
    if (v === 'categories') {
      html = '<p class="legend-title">Drive time to the nearest hospital</p>' + cats.map(function (c) {
        var dim = c.lower < threshold() ? ' dim' : '';
        return '<span class="legend-item' + dim + '"><i style="background:' + c.color + '"></i>' + esc(c.label) + '</span>';
      }).join('') + '<p class="legend-note">Places beyond ' + threshold() + ' min are emphasised.</p>';
    } else if (v === 'travel') {
      html = '<p class="legend-title">Drive time (minutes)</p><div class="legend-ramp" style="background:linear-gradient(90deg,' +
        data.palette.travel.join(',') + ')"></div><div class="legend-scale"><span>0</span><span>60</span><span>120</span><span>180+</span></div>';
    } else {
      html = '<p class="legend-title">People per km\u00b2</p><div class="legend-ramp" style="background:linear-gradient(90deg,' +
        data.palette.population.join(',') + ')"></div><div class="legend-scale"><span>1</span><span>100</span><span>10,000+</span></div>';
    }
    legend.innerHTML = html;
  }

  function renderStats() {
    var t = threshold();
    var sel = province();
    var src = sel === 'all' ? data.national : (provIndex[sel] && provIndex[sel].props);
    if (!src) { stats.innerHTML = ''; return; }
    var name = sel === 'all' ? 'Iran' : esc(sel);
    stats.innerHTML =
      '<div class="stat"><span class="stat-label">' + name + ': within ' + t + ' min</span><span class="stat-value">' + pct(src['share_within_' + t]) + '</span></div>' +
      '<div class="stat"><span class="stat-label">People within ' + t + ' min</span><span class="stat-value">' + num(src['pop_within_' + t]) + '</span></div>' +
      '<div class="stat"><span class="stat-label">Mean drive</span><span class="stat-value">' + num(src.mean_travel_time_min) + ' min</span></div>';
  }

  function setLayer() {
    var v = layerSel.value;
    Object.keys(rasters).forEach(function (k) { if (map.hasLayer(rasters[k])) map.removeLayer(rasters[k]); });
    if (catLayer && map.hasLayer(catLayer)) map.removeLayer(catLayer);
    if (v === 'categories') { if (catLayer) catLayer.addTo(map); } else { rasters[v].addTo(map); }
    renderLegend();
  }

  function setProvince(fit) {
    if (catLayer) catLayer.setStyle(catStyle);
    if (provLayer) provLayer.setStyle(provStyle);
    var sel = province();
    if (fit) {
      if (sel === 'all') map.fitBounds(fullBounds);
      else if (provIndex[sel]) map.fitBounds(provIndex[sel].bounds, {padding: [16, 16]});
    }
    renderStats();
  }

  layerSel.addEventListener('change', setLayer);
  provSel.addEventListener('change', function () { setProvince(true); });
  thrSel.addEventListener('change', function () { setProvince(false); renderLegend(); });
  hospToggle.addEventListener('change', function () {
    if (!hospLayer) return;
    if (hospToggle.checked) hospLayer.addTo(map); else map.removeLayer(hospLayer);
  });

  function getJSON(file) {
    return fetch(base + file).then(function (r) {
      if (!r.ok) throw new Error(file);
      return r.json();
    });
  }

  renderLegend();
  Promise.all([getJSON('travel_time.geojson'), getJSON('province_metrics.geojson'), getJSON('hospitals.geojson')])
    .then(function (res) {
      catLayer = L.geoJSON(res[0], {pane: 'surface', interactive: false, style: catStyle});
      provLayer = L.geoJSON(res[1], {
        pane: 'outlines',
        style: provStyle,
        onEachFeature: function (f, l) {
          var p = f.properties;
          provIndex[p.province] = {props: p, bounds: l.getBounds()};
          l.bindTooltip(function () {
            var t = threshold();
            return '<strong>' + esc(p.province) + '</strong><br>' + pct(p['share_within_' + t]) + ' within ' + t + ' min';
          }, {sticky: true});
          l.on('click', function () { provSel.value = p.province; setProvince(true); });
        }
      }).addTo(map);
      hospLayer = L.geoJSON(res[2], {
        pane: 'sites',
        pointToLayer: function (f, ll) {
          return L.circleMarker(ll, {pane: 'sites', radius: 4, color: '#FFFFFF', weight: 1.2, fillColor: '#13204A', fillOpacity: 1});
        },
        onEachFeature: function (f, l) {
          var p = f.properties;
          l.bindPopup('<strong>' + esc(p.name || 'Unnamed hospital') + '</strong>' +
            (p.name_fa ? '<br><span class="fa">' + esc(p.name_fa) + '</span>' : '') +
            '<br>' + esc(p.province || '') +
            '<br><a href="https://www.openstreetmap.org/' + esc(p.hospital_id) + '" target="_blank" rel="noopener">View on OpenStreetMap</a>');
        }
      });
      if (hospToggle.checked) hospLayer.addTo(map);
      setLayer();
      setProvince(false);
    })
    .catch(function (e) {
      stats.innerHTML = '<p class="map-error">Map layers failed to load (' + esc(e.message) + ').</p>';
    });
}
