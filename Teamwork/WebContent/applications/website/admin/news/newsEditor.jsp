<%@ page import="com.opnlb.website.news.News,
                 com.opnlb.website.news.businessLogic.NewsController,
                 com.opnlb.website.security.WebSitePermissions,
                 org.jblooming.ontology.PersistentFile,
                 org.jblooming.operator.Operator,
                 org.jblooming.persistence.PersistenceHome,
                 org.jblooming.utilities.file.FileUtilities,
                 org.jblooming.waf.ScreenBasic,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.container.ButtonBar,
                 org.jblooming.waf.html.display.DeletePreviewer,
                 org.jblooming.waf.html.display.MultimediaFile,
                 org.jblooming.waf.html.input.*,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState, org.jblooming.waf.html.button.ButtonSupport" %>
<%@ page pageEncoding="UTF-8" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);

  //verify permissions
  Operator logged = pageState.getLoggedOperator();
  if (logged == null || !logged.hasPermissionFor(WebSitePermissions.news_canWrite))
    throw new SecurityException(I18n.get("PERMISSION_LACKING"));

  if (!pageState.screenRunning) {
    ScreenBasic.preparePage(new NewsController(), pageContext);
    pageState.perform(request, response).toHtml(pageContext);

  } else {


    News news = (News) pageState.getMainObject();
    boolean isNew = PersistenceHome.NEW_EMPTY_ID.equals(news.getId());

    PageSeed list = new PageSeed("newsList.jsp");
    list.setCommand(Commands.FIND);
    ButtonLink lista = new ButtonLink(list);
    lista.label = I18n.get("NEWS");

    PageSeed self = pageState.thisPage(request);
    self.mainObjectId = news.getId();
    self.setCommand(Commands.SAVE);
    Form form = new Form(self);
    form.encType = Form.MULTIPART_FORM_DATA;
    form.usePost = true;
    pageState.setForm(form);

    form.start(pageContext);

%><%lista.toHtmlInTextOnlyModality(pageContext);%> /
<h1><%=(news.getTitle() != null ? news.getTitle() : "...")%></h1>

<table class="table">
  <tr>
    <td valign="top" width="30%">
      <table cellpadding="5" cellspacing="3" class="table">

        <tr>
          <td><%

            TextField tfName = new TextField("TEXT", I18n.get("TITLE"), "TITLE", "<br>", 80, false);
            tfName.required = true;
            tfName.maxlength = 255;
            tfName.toHtml(pageContext);

          %></td>
        </tr>
        <tr>
          <td><%

            tfName = new TextField("TEXT", I18n.get("SUBTITLE"), "SUBTITLE", "<br>", 80, false);
            tfName.maxlength = 255;

            tfName.toHtml(pageContext);
          %></td>
        </tr>
        <tr>
          <td valign="top"><%

            TinyMCE txtarea = new TinyMCE(I18n.get("TEXT"), "TEXT", "<br>", "550px", "300px", pageState);
            txtarea.setTheme(TinyMCE.THEME_SIMPLE);

            txtarea.toHtml(pageContext);
          %></td>
        </tr>
      </table>
    </td>
    <td valign="top" width="70%">
      <table width="100%" border="0" cellpadding="5" cellspacing="3">
        <tr>
          <td align="left"><% new CheckField("VISIBLE", "&nbsp;", false).toHtmlI18n(pageContext); %></td>
          <td><%
            DateField df = new DateField("START");
            df.size = 10;
            df.separator = "<br>";
            df.toHtmlI18n(pageContext);
          %></td>
          <td><%
            df = new DateField("END");
            df.separator = "<br>";
            df.size = 10;
            df.toHtmlI18n(pageContext);

          %></td>
          <td><%
            new TextField("TEXT", I18n.get("ORDER_FACTOR"), "ORDER_FACTOR", "<br>", 2, false).toHtml(pageContext);%></td>
        </tr>
        <tr>
          <td valign="top" colspan="2"><%
            Uploader imageFile = new Uploader("IMAGE", pageState);
            imageFile.label = "<b>" + I18n.get("IMAGE") + "</b>";
            imageFile.separator = "<br>";
            imageFile.size = 20;
            imageFile.toolTip = I18n.get("IMAGE");
            imageFile.toHtml(pageContext);

          %></td>
          <td><%
            TextField imgWitdh = new TextField("TEXT", I18n.get("IMG_WIDTH"), "IMG_WIDTH", "<br>", 3, false);
            imgWitdh.maxlength = 3;
            imgWitdh.toHtml(pageContext);
          %></td>
          <td><%
            TextField imgHeight = new TextField("TEXT", I18n.get("IMG_HEIGHT"), "IMG_HEIGHT", "<br>", 3, false);
            imgHeight.maxlength = 3;
            imgHeight.toHtml(pageContext);

          %>
          </td>
        </tr>
        <tr>
          <td valign="top" colspan="4">
              <%
                PersistentFile image = news.getImage();
                if (image != null) {
                  String extension = FileUtilities.getFileExt(image.getOriginalFileName());
                  if(FileUtilities.isImageByFileExt( extension )) {
                   MultimediaFile img = new MultimediaFile(image, request);
                    if (news.getImageWidth() != null && news.getImageWidth() > 0)
                      img.width = news.getImageWidth() + "";
                    img.style = "max-width:500px;max-height:500px";
                    img.toHtml(pageContext);
                  }
                }
             %>
          </td>
        </tr>
      </table>
    </td>
  </tr>
</table>
<%


    ButtonBar bbar = new ButtonBar();

    // save
    ButtonSubmit save = ButtonSubmit.getSaveInstance(form, I18n.get("SAVE"));
    save.label = I18n.get("SAVE");
    save.additionalCssClass = "big first";
    bbar.addButton(save);

    // delete
    if (!isNew) {
      DeletePreviewer deletePreviewer = new DeletePreviewer("PF_DEL", NewsController.class, pageState);
      ButtonSupport delPrev = deletePreviewer.getDeleteButton(I18n.get("DELETE"), news.getId() );
      delPrev.additionalCssClass="big delete";
      bbar.addButton(delPrev);
    }
    bbar.addSeparator(20);


    bbar.toHtml(pageContext);

    pageState.setFocusedObjectDomId(tfName.id);

    form.end(pageContext);
  }

%>