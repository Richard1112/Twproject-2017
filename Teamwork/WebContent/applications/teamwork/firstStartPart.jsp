<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageState" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  TeamworkOperator loggedOp = (com.twproject.operator.TeamworkOperator) pageState.getLoggedOperator();
  Person person = loggedOp.getPerson();
  Img myphoto = person.bricks.getAvatarImage("");
  if(myphoto !=null){
    myphoto.id = "personalAvatar";
  }
%>
<div>
  <label><%=I18n.get("SET_YOUR_IMAGE")%></label>
    <div class="groupRow">
        <div class="groupCell col2 profileImage" style="padding-right: 20px"><%myphoto.toHtml(pageContext);%></div>
        <div class="groupCell col6" style="position: relative; padding-top: 20px" >
                <%
                ButtonJS choose = new ButtonJS(I18n.get("CHOOSE_YOUR_IMAGE"),"openProfileImageEditor('"+person.getId()+"',imageEditorCallback)");
                choose.toHtmlInTextOnlyModality(pageContext);
                %>
        </div>
    </div>
 </div>

<script>

  function openProfileImageEditor(resId,callback){
    openBlackPopup(contextPath+"/applications/teamwork/resource/resourceImageUploader.jsp?RES_ID="+resId,'500px','550px',callback);
  }


  function imageEditorCallback(response){
    //console.debug("imageEditorCallback",response);
    if (response&&response.imageUrl){
      $(".profileImage img").prop("src",response.imageUrl);
      $(".menuTools .avatarImage img").prop("src",response.imageUrl)
    }
  }
</script>
