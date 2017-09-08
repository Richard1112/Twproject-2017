  function lsfOpenEditor(el){
    var ndo=createModalPopup(400,220);
    var div=$("<div>").addClass("lsfEditBox filterName").attr("lsfId",el.attr("lsfId"));
    div.append("<h1>"+i18n.WANT_TO_SAVE_FILTER+"</h1><br>");
    div.append("<label for='NEW_FILTER_NAME'>"+i18n.NEW_FILTER_NAME+"</label><br>");
    div.append("<input type='text' id='NEW_FILTER_NAME' name='NEW_FILTER_NAME' class='formElements' style='width:100%;margin-bottom:10px'><br>");
    div.append("<span onclick='lsfSave($(this));return false;' class='button first'>"+i18n.SAVE+"</span>");
    ndo.append(div);

    var filterName="";
    var ft=$(".filterTitle");
    if (ft.size()>0)
      filterName=ft.text().trim();
    div.find(":text").val(filterName).focus();
  }

  function lsfClick(el){
    $("#FLNMSEL").val('yes');
    $("#FLNM").val(el.text());
    $("#"+el.closest(".customSavedFilters").attr("formId")).submit();
  }

  function lsfSave(el) {
    var lsfEdit = el.closest("[lsfId]");

    var lsf = $("#" + lsfEdit.attr("lsfId"));
    var form = $("#" + lsf.attr("formId"));

    var input = lsfEdit.find(":text:first");

    if (input.val() != "") {
      $("#FLNM").val(input.val());
      var data = {};
      form.find(":input[name]:enabled").each(function() {
        var inp = $(this);
        if (inp.is(":radio")){
          if (inp.is(":checked"))
            data[inp.prop("name")] = inp.val();
        } else if (inp.val() && inp.val() != "" && (inp.prop("name").startsWith("OB_") || !inp.prop("name").startsWith("_"))){
          data[inp.prop("name")] = inp.val();
        }
      });

      data.FLNM=input.val();
      data.CM = "SVFILTER";
      data.cat = lsf.attr("category");

      $.getJSON(contextPath+"/commons/layout/loadSaveFilter/lSFAjaxController.jsp", data, function(response) {
        jsonResponseHandling(response);
        if (response.ok) {

          //openSideBar to show where filter is
          if($.rightPanel.hamburger)
            $.rightPanel.open();

          var message="";
          if (response.filterUpdated){

            setTimeout(function(){

              lsf.find("[filterName='"+input.val()+"'] .button").effect("highlight", { color: "#F9EFC5" }, 3000);
            },600);

            message = i18n.FILTER_UPDATED;

          } else {
            var newFilterBtn = lsfCreateButton({name:input.val()});
            lsf.prepend(newFilterBtn);

            setTimeout(function(){
              newFilterBtn.effect("highlight", { color: "rgba(249, 239, 197, 0.8)" }, 3000);
            },600);
            message = i18n.FILTER_SAVED;
          }
          input.val("");
          closeBlackPopup();

          showFeedbackMessage("OK",message);
        }
      });
    }

  }

  function lsfDelete(el){
    //console.debug("lsfDelete",el);
    var lsf = el.closest(".customSavedFilters");
    var btn=el.closest("[filterName]");

    var data={
      CM:"RMFILTER",
      cat:lsf.attr("category"),
      FLNM:btn.attr("filterName")
    };

    $.getJSON(contextPath+"/commons/layout/loadSaveFilter/lSFAjaxController.jsp",data,function(response){
      jsonResponseHandling(response);
      if (response.ok){
        btn.fadeOut(300, function(){
          btn.remove();
        });
        $("#FLNM").val("");
        $("#FLNMSEL").val("");
      }
    });
  }

  function lsfCreateButton(data) {
    var spanExt = $("<span>").addClass("customFilterElement").attr("filterName", data.name);
    var spanBtn = $("<span>").addClass("button textual noprint" + (data.selected ? " focused" : "")).click(function () {lsfClick($(this))}).append(data.name);
    var spanIco = $("<span>").addClass("teamworkIcon delete noprint" + (data.selected ? " focused" : "")).click(function () {
      $(this).confirm(function () {lsfDelete($(this))}, i18n.FLD_CONFIRM_DELETE);
      return false;
    }).attr("title", i18n.DELETE).append("d");

    spanExt.append(spanBtn).append(spanIco);
    return spanExt;

  }
