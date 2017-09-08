<%@ page import="com.twproject.waf.TeamworkHBFScreen,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.html.input.Uploader,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.ApplicationState,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    final ScreenArea body = new ScreenArea(request);
    TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
    lw.register(pageState);
    pageState.perform(request, response);
    pageState.toHtml(pageContext);
  } else {

    // Form -----------------------------------------------------------------------------------------------
    PageSeed self = pageState.thisPage(request);

    Form f = new Form(self);
    f.encType = Form.MULTIPART_FORM_DATA;
    f.alertOnChange = true;
    pageState.setForm(f);

    f.start(pageContext);


%>
<div style="width: 300px;">
<div id="holder" class="uploadizeDrop" ></div>

<div id="filelist" style="border: 5px dashed red;min-height: 100px;width: 300px;" ></div>

<div id="qui" style="height: 200px; overflow: auto;"></div>
</div>
<script>

  $(function(){
    $("#holder").uploadize({
      url:"upload.jsp",
      preview:true,
      //fileAreaSelector:"#filelist",
      maxSize:0,
      //maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
      onLoadCallback:function(response){
        //console.debug("mio",response)
      }
    });
  });

</script>




<%


    f.end(pageContext);
  }
%>