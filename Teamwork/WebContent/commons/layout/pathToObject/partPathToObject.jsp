<%@ page import="com.twproject.task.Task,
                 org.jblooming.ontology.PerformantNodeSupport,
                 org.jblooming.operator.Operator,
                 org.jblooming.security.Securable,
                 org.jblooming.utilities.JSP,
                 org.jblooming.utilities.StringUtilities,
                 org.jblooming.waf.constants.Commands,
                 org.jblooming.waf.html.button.ButtonJS,
                 org.jblooming.waf.html.button.ButtonLink,
                 org.jblooming.waf.html.container.DivOnMouseover,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.display.PathToObject,
                 org.jblooming.waf.view.PageState,
                 java.util.Iterator,
                 java.util.List, com.twproject.waf.html.StatusIcon, org.jblooming.waf.settings.I18n" %>
<%

  PageState pageState = PageState.getCurrentPageState(request);
  PathToObject pathToObject = (PathToObject) JspIncluderSupport.getCurrentInstance(request);

if (PathToObject.DRAW.equals(request.getAttribute(PathToObject.ACTION))) {


%>
<div class="pathToObject"><%

  Operator logged = pageState.getLoggedOperator();
  PerformantNodeSupport node = pathToObject.node;
  boolean isSecurable = node instanceof Securable;

  if (node != null) {
    if (pathToObject.rootDestination != null) {
      pathToObject.rootDestination.enabled = true;
      pathToObject.rootDestination.toHtmlInTextOnlyModality(pageContext);
%><%
  }

  if (pathToObject.destination == null) {
    pathToObject.destination = pageState.thisPage(request);
    pathToObject.destination.setCommand(Commands.EDIT);
  }

  if (pathToObject.selfDestination == null)
    pathToObject.selfDestination = pathToObject.destination;

  boolean isNew = node.isNew();

  List<PerformantNodeSupport> ancestors = node.getAncestors();
  int ancestorSize = ancestors.size();

  // ------------------------------------------------------------------ LOOP ANCESTORS
  for (int i = 0; i < ancestorSize - 1; i++) {
    PerformantNodeSupport anc = ancestors.get(i);
    //peak of elegance
    if (isSecurable && (pathToObject.canClick != null && !((Securable) anc).hasPermissionFor(logged, pathToObject.canClick)))
      continue;

    // ------------------------ ancestor
    String label = anc.getName();
    label = JSP.encode(label);

    if (!JSP.ex(pathToObject.alternativeCEForMainObject)) {
      pathToObject.destination.mainObjectId = anc.getId();
    } else {
      pathToObject.destination.addClientEntry(pathToObject.alternativeCEForMainObject, anc.getId());
    }
    ButtonLink edit = new ButtonLink(label, pathToObject.destination);
    edit.noPrint = false;
    //edit.additionalCssClass = "ancestor";
    edit.toHtmlInTextOnlyModality(pageContext);

    drawChildrenMenu(anc, pathToObject, logged, isSecurable, pageContext);


    // ------------------------ separator
    if (i < ancestorSize - 2) {
%><span class="pathSeparator"><%=pathToObject.separator%></span><%
    }
  }

%>
  <div class="currentNode">
    <%
      if (pathToObject.currentNodeLeftElement!=null)
        pathToObject.currentNodeLeftElement.toHtml(pageContext);
    %>
    <%
      if (!isNew) {
        // ------------------ edit current node
        String label = JSP.encode(node.getName());
        if (!JSP.ex(pathToObject.alternativeCEForMainObject)) {
          pathToObject.selfDestination.mainObjectId = node.getId();
        } else {
          pathToObject.selfDestination.addClientEntry(pathToObject.alternativeCEForMainObject, node.getId());
        }

    %><%
    ButtonLink edit = new ButtonLink(label, pathToObject.selfDestination);
    edit.noPrint = false;
    edit.additionalCssClass = "objectName";
    edit.toHtmlInTextOnlyModality(pageContext);

    drawChildrenMenu(node, pathToObject, logged, isSecurable, pageContext);

  } else {
    %><i><%=JSP.w(pathToObject.displayNameForNewObject)%></i><%
  }

  %></div>
  </div>
<%
    }
  }

%><%!
  private void drawChildrenMenu(PerformantNodeSupport currentNode, PathToObject pathToObject, Operator logged, boolean isSecurable, PageContext pageContext) {
    // ------------------------ ancestor' child
    if (currentNode.getChildrenSize() > 0) {
      Iterator it;
      if (pathToObject.comparator == null)
        it = currentNode.getChildrenIteratorByName();
      else
        it = currentNode.getChildrenIterator(pathToObject.comparator);

      int counter = 1;

      ButtonJS opener = new ButtonJS("");
      opener.iconChar = "&ugrave;";
      DivOnMouseover chdDomo = new DivOnMouseover(opener);

      while (it.hasNext()) {
        PerformantNodeSupport child = (PerformantNodeSupport) it.next();
        if (isSecurable && (pathToObject.canClick != null && !((Securable) child).hasPermissionFor(logged, pathToObject.canClick)))
          continue;

        if (!JSP.ex(pathToObject.alternativeCEForMainObject)) {
          pathToObject.destination.mainObjectId = child.getId();
        } else {
          pathToObject.destination.addClientEntry(pathToObject.alternativeCEForMainObject, child.getId());
        }
        ButtonLink chb = ButtonLink.getTextualInstance(child.getName(), pathToObject.destination.getNewInstance());
        chb.noPrint = false;
        chb.enabled = !child.equals(pathToObject.node);
        chdDomo.addButton(chb);
        counter++;
      }
      if (counter > 1) {
        chdDomo.toHtmlInTextOnlyModality(pageContext);
      }
    }
  }
%>
