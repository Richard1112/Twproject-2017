<%@ page import="com.twproject.security.TeamworkArea,
                 org.jblooming.operator.Operator,
                 org.jblooming.persistence.objectEditor.FieldFeature,
                 org.jblooming.persistence.objectEditor.ObjectEditor,
                 org.jblooming.persistence.objectEditor.businessLogic.ObjectEditorController,
                 org.jblooming.security.Area,
                 org.jblooming.waf.ScreenBasic
                 , org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.SmartCombo, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%>
<%
    PageState pageState = PageState.getCurrentPageState(request);

    if (!pageState.screenRunning) {

      Operator logged =  pageState.getLoggedOperator();
      logged.testIsAdministrator();



      ObjectEditor objectEditor= new ObjectEditor(I18n.get("AREA_MANAGEMENT"), TeamworkArea.class, pageContext);

      String hql = "select operator.id, operator.name || ' ' || operator.surname from " + Operator.class.getName() + " as operator ";

      String whereForId = "where operator.id = :" + SmartCombo.FILTER_PARAM_NAME;

      String whereForFiltering =
              " where operator.name || ' ' || operator.surname like :" + SmartCombo.FILTER_PARAM_NAME +
                      " or operator.surname || ' ' || operator.name like :" + SmartCombo.FILTER_PARAM_NAME;

      SmartCombo ops = new SmartCombo("owner", hql, whereForFiltering, whereForId);


      // list part
      {

      objectEditor.query="from "+TeamworkArea.class.getName() + " as area order by area.id";
      objectEditor.mainHqlAlias = "area";


      objectEditor.addDisplayField("name", I18n.get("NAME"));

      FieldFeature ffo = new FieldFeature("owner",I18n.get("OWNER"));
      ffo.smartCombo = ops;
      ffo.smartComboClass = Operator.class;
      objectEditor.addDisplayField(ffo);

      }{

      //edit part
      FieldFeature codeEd = new FieldFeature("name",I18n.get("NAME"));
      codeEd.required = true;
      objectEditor.addEditField(codeEd);

      FieldFeature ffo = new FieldFeature("owner",I18n.get("OWNER"));
      ffo.smartCombo = ops;

      objectEditor.addEditField(ffo);

      }

      ScreenBasic screenBasic = ScreenBasic.preparePage(new ObjectEditorController(objectEditor), pageContext);
      screenBasic.getBody().areaHtmlClass="lreq30 lreqPage";
      pageState.perform(request, response);
      if (pageState.getMainObject() !=null && !((Area) pageState.getMainObject()).isNew()) {

              PageSeed thisP = pageState.pageFromRoot("security/roleEditor.jsp");
              thisP.command = Commands.ADD;
              thisP.addClientEntry("AREA", pageState.getMainObject().getId());
              ButtonLink addRole = new ButtonLink(I18n.get("ADD_ROLE_THIS_AREA"),thisP);
              objectEditor.additionalButtons.add(addRole);

              PageSeed thisL = pageState.pageFromRoot("security/roleList.jsp");
              thisL.command = Commands.FIND;
              thisL.addClientEntry("AREA", pageState.getMainObject().getId());
              ButtonLink seeRoles = new ButtonLink(I18n.get("SEE_ROLES_THIS_AREA"),thisL);
              objectEditor.additionalButtons.add(seeRoles);
      }

      pageState.toHtml(pageContext);

    } else {
      %><script>$("#ADMINISTRATION_ROOT_MENU").addClass('selected');</script><%
      pageState.getMainJspIncluder().toHtml(pageContext);
    }

%>