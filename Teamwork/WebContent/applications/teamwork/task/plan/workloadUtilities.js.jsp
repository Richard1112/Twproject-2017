<%@ page import="org.jblooming.waf.settings.I18n, org.jblooming.security.License" %>
<script>


function computeResourceLoad(el,period, resId, currentWorkload,assId) {
  //console.debug("computeResourceLoad",period, resId, currentWorkload,assId);

  if (!period || <%=!License.assertLevel(20)%>)
    return;

  var row = el.closest("tr");
  //si chiama con delay sulla riga in modo da
  row.stopTime("loadOpLoad");
  var workloadInfo = row.find(".workloadInfo");
  workloadInfo.removeClass("loading").empty();

  if (resId) {
    workloadInfo.addClass("loading");
    row.oneTime(1000, "loadOpLoad", function () {
      getOperatorLoad(resId, period.startMillis, period.endMillis, currentWorkload, assId, function (response) {
        if (response.ok && response.stats) {
          var bl = $("<div>").attr({"data-timer": 0, "data-forceposition": "left"});
          bl.attr("data-balloon", "<b><%=I18n.get("WORKLOAD_ON_PERIOD")%></b><br><span class='textSmall'>" +
              (new Date().format()) + " : " + (new Date(period.endMillis).format()) + "<br>" +
              "<%=I18n.get("WORKLOAD")%> - min: " + getMillisInHoursMinutes(response.stats.minLoad) + "  max: <b>" + getMillisInHoursMinutes(response.stats.maxLoad) + "</b><br>" +
              "<%=I18n.get("MEAN_WORK_CAPACITY")%>: " + getMillisInHoursMinutes(response.stats.meanCapacity) + "<br>"
          );
          bl.css("cursor", "pointer");
          if (response.stats.meanLoadPerc > 100)
            bl.addClass("textSmall warning warningIcon");
          bl.append(parseInt(response.stats.meanLoadPerc) + "%");
          workloadInfo.empty().append(bl);
        } else {
          workloadInfo.empty();
        }
        workloadInfo.removeClass("loading");
      });
    })
  }
}



function getOperatorLoad(resId, startMillis, endMillis, currentWL, assId,callback) {
  //console.debug("getOperatorLoad",resId, startMillis, endMillis, currentWL, assId);
//se il progetto doveva già essere chiuso non si calcola niente
  if (endMillis < new Date().getTime()) {
    if (typeof (callback) == "function") {
      callback({ok: false});
    }
    return;
  }

  var request = {
    CM:            "GETOPLOAD",
    resId:         resId,
    startMillis:   startMillis,
    endMillis:     endMillis,
    simulatedLoad: currentWL,
    assId:assId
  };
  $.getJSON(contextPath + "/applications/teamwork/task/plan/planAjaxController.jsp", request, function (response) {
    jsonResponseHandling(response);
    if (typeof (callback) == "function") {
      callback(response);
    }
  })
}
</script>