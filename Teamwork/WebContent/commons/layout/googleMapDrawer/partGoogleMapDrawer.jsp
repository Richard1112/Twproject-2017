<%@ page import=" org.jblooming.utilities.JSP,
                  org.jblooming.waf.html.core.JspIncluderSupport,
                  org.jblooming.waf.html.display.GoogleMapDrawer,
                  org.jblooming.waf.view.PageState, java.util.List"%><%@page pageEncoding="UTF-8" %><%

  PageState pageState = PageState.getCurrentPageState(request);
  GoogleMapDrawer drawer = (GoogleMapDrawer) JspIncluderSupport.getCurrentInstance(request);

  // if activation key is missing nothing done!
  if (JSP.ex(drawer.activationKey)) {   
    if (GoogleMapDrawer.INITIALIZE.equals(request.getAttribute(GoogleMapDrawer.ACTION))) {
    %>
      <script src="http://maps.google.com/maps?file=api&amp;v=2&amp;key=<%=drawer.activationKey%>" type="text/javascript"></script>
      <script src="<%=request.getContextPath()+"/commons/layout/googleMapDrawer/googlemap.js"%>" type="text/javascript"></script>
      <script src="<%=request.getContextPath()+"/commons/layout/googleMapDrawer/addresschooser.js"%>" type="text/javascript"></script><%

} else if (GoogleMapDrawer.DRAW_MAP.equals(request.getAttribute(GoogleMapDrawer.ACTION))) {
%>
      <input type='hidden' name='lat' id='lat'/>
      <input type='hidden' name='lng' id='lng'/>
      <div id='map_container'>
        <div id="big_spinner"></div>
        <div id='map'></div>
      </div>
      <style type="text/css">
        #map_container {
          width: <%=drawer.width%>px;
        }
        #map {
          width: <%=drawer.width%>px;
          height: <%=drawer.height%>px;
          overflow: hidden;
            color:#000000;
            text-align:left;
        }
          
      </style>
      <script type='text/javascript'>
         widget = new Mapeed.AddressChooser.Widget({
            onInitialized: onInitialized ,
            zip:             '<%=drawer.listenedZipEntry%>',
            street:          '<%=drawer.listenedStreetEntry%>',
            city:            '<%=drawer.listenedCityEntry%>',
            state:           '<%=drawer.listenedProvinceEntry%>',
            country:         '<%=drawer.listenedCountryEntry%>'
          });

        // If you want to customized your map, add code in onInitialized callback
        function onInitialized(widget) {
          widget.getMap().addControl(new GSmallMapControl());
          // If input fields have values, it'll display current position on the map otherwise it'll center map on user location
          widget.initMap();
        }
      </script><%


    } else if (GoogleMapDrawer.DRAW_STATIC_MAP.equals(request.getAttribute(GoogleMapDrawer.ACTION))) {

      String mapTypes = "";
      if (JSP.ex(drawer.geoMapTypes)) {
        for (String type : drawer.geoMapTypes) 
          mapTypes += type + ",";
        if (mapTypes.endsWith(","))
          mapTypes = mapTypes.substring(0, mapTypes.length() - 1);
      } else
        mapTypes = "G_NORMAL_MAP, G_PHYSICAL_MAP, G_HYBRID_MAP";

      List<GoogleMapDrawer.Marker> markersList = drawer.markers;
      if (JSP.ex(markersList)) {
    %><script type="text/javascript">
        var datasArray = new Array();
        function initialize(datasArray) {
          /**
           * map
           */
          var map = new GMap2(document.getElementById("map_canvas"), {mapTypes:[<%=mapTypes%>]});
          //map.addControl(new GLargeMapControl());
          //map.addControl(new GMapTypeControl(1));
          //map.addControl(new GScaleControl());
          map.getDefaultUI();
          map.closeInfoWindow(); //preload iw

          /**
           * Geocoder
           */
          var points = [];
          var bounds = new GLatLngBounds();
          var geocoder = new GClientGeocoder();
          var lines;
          var lineNumber;
          var len = 0;
          var bar = document.getElementById("bar");
          var start = new Date().getTime();
          len = datasArray.length;
          lineNumber=0;

          /**
           * zoom and pan to fit in view
           */
          GMap2.prototype.fit = function(bounds){
            this.setCenter(bounds.getCenter(), this.getBoundsZoomLevel(bounds));
          }

          <%
            for (GoogleMapDrawer.Marker marker : markersList) {
          %>
            //for(var i in datasArray) {
              //var stringa = datasArray[i].split(',');
              var lat = '<%=marker.latitude%>';
              var lng = '<%=marker.longitude%>';
              var point = new GLatLng(lat, lng);

              markerOptions = '';

              <%
              if(JSP.ex(marker.iconUrl)) {
              %>
                var icon = new GIcon();
                icon.image = '<%=marker.iconUrl%>'
                //icon.iconSize = new GSize(32, 32); // google default
                icon.iconSize = new GSize(20, 34);
                icon.shadow = contextPath + "/applications/webwork/images/googleMaps/shadow.png";
                //icon.iconAnchor = new GPoint(16, 16);  // google default
                // icon.infoWindowAnchor = new GPoint(25, 7); // google default
                icon.shadowSize = new GSize(38.0, 34.0);
                icon.iconAnchor = new GPoint(10.0, 17.0);
                icon.infoWindowAnchor = new GPoint(10.0, 17.0);
                // Set up our GMarkerOptions object
                markerOptions = { icon:icon};
              <%
              }
              %>

              var marker = new GMarker(point, markerOptions);
              map.addOverlay(marker);

              var data ='<%=JSP.javascriptEncode(marker.infoWindowString)%>';
              if(data)
                marker.bindInfoWindowHtml(data);
              bounds.extend(point);
              map.fit(bounds);
          <%
          }
          %>
        }        
        $(function(){ initialize(datasArray) })
      </script>
      <div id="map_canvas" style="width:<%=drawer.width%>px; height:<%=drawer.height%>px;"></div><%
      }
    }
  } else {
  %>&nbsp;<%      
  }

%>