<%@ page import="org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.ColorChooser,
                 org.jblooming.waf.html.input.TextField, org.jblooming.waf.settings.I18n"%><%

  ColorChooser cc = (ColorChooser)JspIncluderSupport.getCurrentInstance(request);

%><label for="<%=cc.fieldName%>"><%=JSP.w(cc.label)%></label><%=JSP.w(cc.separator)%>
    <span class="colorChooserGroup"><%
      TextField hidden = new TextField(cc.showTextField?"text":"text",cc.fieldName,"",2);
      hidden.label="";
      hidden.fieldClass=hidden.fieldClass+" colorChooserField";
      hidden.readOnly=cc.disabled;
      hidden.toHtml(pageContext);
    %>
    </span>
<script type="text/javascript">

  var defs = [
    initialize(contextPath + "/commons/js/jquery/spectrum/spectrum.css", "css"),
    initialize(contextPath + "/commons/js/jquery/spectrum/tw_spectrum.css", "css"),
    initialize(contextPath + "/commons/js/jquery/spectrum/spectrum.js", "script")
  ];

  $.when.apply(null, defs).done(function () {
    /*
     SPECTRUM color chooser
     https://bgrins.github.io/spectrum/#why-customizable

     color: tinycolor,
     flat: bool,
     showInput: bool,
     showInitial: bool,
     allowEmpty: bool,
     showAlpha: bool,
     disabled: bool,
     localStorageKey: string,
     showPalette: bool,
     showPaletteOnly: bool,
     togglePaletteOnly: bool,
     showSelectionPalette: bool,
     clickoutFiresChange: bool,
     cancelText: string,
     chooseText: string,
     togglePaletteMoreText: string,
     togglePaletteLessText: string,
     containerClassName: string,
     replacerClassName: string,
     preferredFormat: string,
     maxSelectionSize: int,
     palette: [[string]],
     selectionPalette: [string]
     */


      $("#<%=cc.fieldName%>").spectrum({
        color: $("#<%=cc.fieldName%>").val(),
        allowEmpty: true,
        palette: <%=cc.palette%>,
        showInput: <%=cc.showTextField%>,
        showPaletteOnly: <%=cc.showOnlyPalette%>,
        showPalette: <%=cc.showPalette%>,
        hideAfterPaletteSelect:false,
        preferredFormat: "hex",

        show: function(color) {
          $("#<%=cc.fieldName%>").spectrum("option", "hideAfterPaletteSelect",  true);

          if(<%=cc.showOnlyPalette%>){

            var clearButton = $("<div class='clear-palette' title='Clear color'><%=I18n.get("DELETE")%></div>");
            clearButton.on("click.spectrum", function (e) {
              e.preventDefault();
              e.stopPropagation();
              $(".sp-clear").click();
              $("#<%=cc.fieldName%>").val("").spectrum("hide");
            });
            $('.sp-palette').append(clearButton);

          }
        }

      });

  });
</script>


