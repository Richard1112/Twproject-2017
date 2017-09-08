<%@ page import="org.jblooming.ontology.PersistentFile,
                 org.jblooming.tracer.Tracer,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.Img,
                 org.jblooming.waf.html.input.Uploader,
                 org.jblooming.waf.view.ClientEntry,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, org.jblooming.utilities.file.FileUtilities, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.button.ButtonJS"%><%

  Uploader upl = (Uploader) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);

  if (JSP.ex(upl.label))
    out.println(JSP.makeTag("label", "for=\""+upl.id+"\" class=\""+(upl.required?"required":" ")+(JSP.w(upl.classLabelName))+"\"" , upl.label + (upl.required && !upl.label.contains("*")?"*":"")));

  if (upl.className == null || upl.className.trim().length() == 0) {
    upl.className = "formElements";
  }

  ClientEntry entry = pageState.getEntry(upl.fieldName);
  String uplUID =entry.stringValueNullIfEmpty();
  uplUID = JSP.w(uplUID);


%><%=upl.separator%><table border="0" width="<%=upl.size+10%>" class="upvtable" uid="<%=uplUID.replaceAll("'", "&rsquo;")%>" cellpadding="0" cellspacing="0"><tr><td nowrap><%

  boolean showUpload = !JSP.ex(uplUID);
  boolean fileDisabled = !showUpload || upl.disabled ;
  %>
  <span class="sp_fi" style="display:<%=(showUpload ? "":"none;")%>">
    <input type="file"  <%=upl.preserveOldValue?"oldvalue=\"1\"":""%> name="<%=upl.fieldName%>" id="<%=upl.id%>" size="<%=upl.size%>" class="<%=upl.className%>" <%=upl.generateToolTip()%> <%=(upl.readOnly || fileDisabled ? " disabled" : "")%> <%=(upl.jsScript != null && upl.jsScript.length() > 0 ? ' ' + upl.jsScript : "")%><%
    if (upl.launchedJsOnActionListened != null && !fileDisabled) {
      %><%=upl.actionListened%>= "if (event.keyCode==<%=upl.keyToHandle%>) { <%=upl.launchedJsOnActionListened%> }"<%
    }
   %>><%

  if(showUpload) {
    out.print("</td> <td nowrap>");
  }

    String spanlabel = "";
    if (JSP.ex(uplUID)) {
      PersistentFile persistentFile = null;

      //shitty behaviour on invalid entries - hence patched with try-catch
      try {
        persistentFile = PersistentFile.deserialize(uplUID);
      } catch (Throwable t) {
        Tracer.platformLogger.warn(t);
      }

      if (persistentFile!=null) {
        spanlabel = persistentFile.getOriginalFileName();

        /*
        PageSeed myself = persistentFile.getPageSeed(upl.treatAsAttachment );
        ButtonLink doc = new ButtonLink(myself);
        doc.target = "_blank";
        doc.width=""+upl.size;
        doc.label = spanlabel;
        doc.enabled=!JSP.ex(entry.errorCode);
        */

        ButtonSupport doc;
        if (FileUtilities.isImageByFileExt(persistentFile.getFileExtension()) || FileUtilities.isPdfByFileExt(persistentFile.getFileExtension())){
          doc=new ButtonJS("openBlackPopup('"+persistentFile.getPageSeed(false).toLinkToHref()+"')");
        } else {
          doc= new ButtonLink(persistentFile.getPageSeed(true));
          doc.target="_blank";
        }

        doc.width=""+upl.size;
        doc.label = spanlabel;
        doc.enabled=!JSP.ex(entry.errorCode);


        %></span>

        <div title="<%=uplUID%>" id="sp_tf_<%=upl.id%>" style="display:<%=(showUpload ? "none;":"")%>; overflow: hidden; max-width: <%=upl.size*12%>px;" class="sp_tf <%=upl.className == null ? "": upl.className %>">
          <%doc.toHtmlInTextOnlyModality(pageContext);%>
        </div> <%
      }
    }
  if (!showUpload ) {
    %></td><td><%
    Img act = new Img("uploader/" + (showUpload ? "link.png" : "unlink.png"), "");
    act.align = "absmiddle";
    act.style="cursor:pointer; width:15px; margin-left:5px";

    if (!upl.disabled && !upl.readOnly) {
      act.script = "onClick='uplClickManage($(this));'";
    }
    act.toHtml(pageContext);
  }

  if (upl.doFeedBackError) {
    %></td><td><%
     JSP.feedbackError(entry, upl.translateError, pageContext);
  }

%></td></tr>
</table>
