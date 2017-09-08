<%@ page import="com.opnlb.website.page.WebSitePage,
                 org.jblooming.operator.Operator,
                 org.jblooming.oql.OqlQuery,
                 org.jblooming.persistence.PersistenceHome,

                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.exceptions.ActionException,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.button.ButtonSubmit,
                 org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,
                 org.jblooming.waf.settings.I18n,
                 org.jblooming.waf.view.PageSeed,
                 org.jblooming.waf.view.PageState,
                 java.util.HashSet,
                 java.util.List,
                 java.util.TreeSet, org.jblooming.ontology.PlatformComparators" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);
  Operator logged = pageState.getLoggedOperator();



  boolean postLink = false;
  if (Commands.DELETE.equals(pageState.getCommand())) {

    Operator op = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, logged.getId());
    String label = pageState.mainObjectId + "";
    op.getFavoriteUrls().remove(label);
    op.store();
    logged.getFavoriteUrls().remove(label);

  } else if (Commands.SAVE.equals(pageState.getCommand())) {
    try {
      Operator op = (Operator) PersistenceHome.findByPrimaryKey(Operator.class, logged.getId());
      String title = pageState.getEntryAndSetRequired("TITLE").stringValue();
      String url = pageState.getEntryAndSetRequired("URL").stringValue();

      for (String currentFilterName : new HashSet<String>(op.getFavoriteUrls().keySet())) {
        if (title.toLowerCase().equals(currentFilterName.toLowerCase())) {
          op.getFavoriteUrls().remove(currentFilterName);
        }
      }

      op.getFavoriteUrls().put(title, url);
      op.store();
      logged.getFavoriteUrls().put(title, url);

    } catch (ActionException a) {

    }
  } else if ("POSTLINK".equals(pageState.getCommand())) {
    postLink = true;
  }

%>

<div id="managFavDiv" class="bookmarksDiv portletBox small">
  <style >

    .bookmarksDiv h2, .customPagesDiv h2 {
      padding:0;
      border:0
    }


    .customPagesDiv .button.textual {
      color: #617777;
      display: block;
      font-size: 14px;
      line-height: 24px;
      text-align: left;
    }

    .bookmarksDiv .linkLine, .customPagesDiv .linkLine {
      position: relative;
      border-bottom: 1px solid #CCCCCC;
    }

    .linkLine .button.textual{
      line-height: 30px;
    }

  </style>
  <%

    PageSeed self = pageState.pagePart(request);
    //self.setCommand(Commands.FIND);
    self.mainObjectId="dummy";
    Form f = new Form(self);
    f.start(pageContext);

/*
________________________________________________________________________________________________________________________________________________________________________


  FAVORITES

________________________________________________________________________________________________________________________________________________________________________

*/


  %>

  <div>

    <h1><%=I18n.get("FAVORITES")%></h1>
    <div><%

      if (!request.getRequestURI().contains("manageFavorites.jsp")) {
        ButtonLink manLin = new ButtonLink(pageState.pageFromRoot("tools/manageFavorites.jsp"));
        manLin.label = I18n.get("MANAGE_FAVORITES");
        manLin.toHtmlInTextOnlyModality(pageContext);

    %> - <%
      }

      ButtonJS addLinkJS=new ButtonJS("$('#addDiv').toggle()");
  /*addLinkJS.iconChar="P";*/
      addLinkJS.additionalCssClass="edit";
      addLinkJS.label=I18n.get("ADD_LINK");
      addLinkJS.toHtmlInTextOnlyModality(pageContext);
    %>
    </div>

    <div id="addDiv" style="display:<%=postLink?"block":"none"%>">
      <table class="table">
        <tr>
          <td ><%

            TextField title = new TextField("TITLE", "<br>");
            title.label = I18n.get("FAVORITES_TITLE");
            title.fieldSize=30;
            title.script="style=width:100%";
            title.required = true;
            title.toHtml(pageContext);
          %></td></tr><tr><td><%
        TextField url = new TextField("URL", "<br>");
        url.fieldSize=30;
        url.script="style=width:100%";
        url.label = I18n.get("FAVORITES_URL");
        url.required = true;
        url.toHtml(pageContext);

      %></td></tr><tr>
        <td><%
          ButtonSubmit sav = ButtonSubmit.getAjaxButton(f, "managFavDiv");
          //sav.additionalOnClickScript="history.back();";
          sav.variationsFromForm.command = Commands.SAVE;
          sav.label = I18n.get("ADD");
          sav.additionalCssClass="small";
          sav.toHtml(pageContext);
        %></td>
      </tr>
      </table><div class="separator"></div></div>

    <%

      if (logged.getFavoriteUrls() != null && logged.getFavoriteUrls().size() > 0) {

        TreeSet<String> ts = new TreeSet(new PlatformComparators.IgnoreCaseComparator());
        ts.addAll(logged.getFavoriteUrls().keySet());

        for (String label : ts) {
          PageSeed link = new PageSeed(logged.getFavoriteUrls().get(label));
          ButtonLink line = new ButtonLink(label, link);
          line.additionalCssClass = "small";
          line.inhibitParams = true;
          line.target = "_blank";
          ButtonSubmit del = ButtonSubmit.getAjaxButton(f, "managFavDiv");
          del.variationsFromForm.command = Commands.DELETE;
          del.variationsFromForm.setMainObjectId(label);
          del.additionalCssClass="delete";
          del.iconChar="d";
          del.label = "";
          del.toolTip = I18n.get("REMOVE");
          del.confirmRequire = true;

    %><div class="linkLine"><div style="float:right"> <%del.toHtmlInTextOnlyModality(pageContext);%></div><%line.toHtmlInTextOnlyModality(pageContext);%></div><%
      }

    }
  %>
  </div>
  <%




    f.end(pageContext);

  %><script type="text/javascript">
  if ("<%=Commands.SAVE%>"=="<%=pageState.command%>"){
    location.href="<%=pageState.getEntry("URL").stringValue()%>";
  }
</script></div>


