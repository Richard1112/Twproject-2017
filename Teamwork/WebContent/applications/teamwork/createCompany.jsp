<%@ page contentType="text/html; charset=utf-8" pageEncoding="UTF-8" %>
<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.resource.Company, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.security.TeamworkArea,
com.twproject.security.TeamworkPermissions, com.twproject.waf.TeamworkHBFScreen, org.jblooming.anagraphicalData.AnagraphicalData, org.jblooming.oql.OqlQuery, org.jblooming.security.Area,
 org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.OperatorConstants, org.jblooming.waf.html.button.ButtonLink,
 org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.html.container.ButtonBar, org.jblooming.waf.html.input.TextField, org.jblooming.waf.html.state.Form,
 org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List" %><%


  PageState pageState = PageState.getCurrentPageState(request);
    TeamworkOperator loggedOp = (com.twproject.operator.TeamworkOperator) pageState.getLoggedOperator();
    Person person = loggedOp.getPerson();

    if (!pageState.screenRunning) {

        pageState.screenRunning = true;
        final ScreenArea body = new ScreenArea(request);
        TeamworkHBFScreen lw = new TeamworkHBFScreen(body);
        lw.register(pageState);
        pageState.perform(request, response);

        if (Commands.SAVE.equals(pageState.command)) {
            Resource company = null;
            String companyName = pageState.getEntry("COMPANY_NAME").stringValueNullIfEmpty();
            String name = pageState.getEntry(OperatorConstants.FLD_NAME).stringValueNullIfEmpty();
            String surname = pageState.getEntry(OperatorConstants.FLD_SURNAME).stringValueNullIfEmpty();
            String email = pageState.getEntry("EMAIL").stringValueNullIfEmpty();

            if (JSP.ex(name))
                person.setPersonName(name);

            if (JSP.ex(surname))
                person.setPersonSurname(surname);

            AnagraphicalData ad = null;
            if (person.getAnagraphicalDatas().size() == 0) {
                ad = new AnagraphicalData();
                ad.setIdAsNew();
                ad.store();
                person.getAnagraphicalDatas().add(ad);
            } else {
                ad = person.getAnagraphicalDatas().iterator().next();
            }

            if (JSP.ex(email)) {
                ad.setEmail(email);
            }

            if (JSP.ex(companyName)) {
                company = new Company();
                company.setIdAsNew();
                company.setOwner(loggedOp);
                company.setName(companyName);
                company.store();

                ad = new AnagraphicalData();
                ad.setIdAsNew();
                ad.store();
                company.getAnagraphicalDatas().add(ad);

                //reset default area name
                String hql = "from " + TeamworkArea.class.getName();
                List areas = new OqlQuery(hql).list();
                if (areas != null && areas.size() == 1) {
                    Area a = (Area) areas.get(0);
                    a.setName(companyName);
                    a.store();

                    //set area on logged, company
                    company.setArea(a);
                    company.store();
                    loggedOp.getPerson().setArea(a);
                    loggedOp.getPerson().store();
                }
            }

            if (company != null)
                loggedOp.getPerson().setParentAndStore(company);

            loggedOp.store();

            if (pageState.validEntries()) {
                pageState.getLoggedOperator().putOption(com.opnlb.website.waf.WebSiteConstants.HOME_PAGE, "getsThingsDone.page");
                pageState.getLoggedOperator().store();
                pageState.setCommand("ACCOUNT_SET");
            }
        }

        pageState.toHtml(pageContext);

    } else {

%>
<script>$("#HOME_MENU").addClass('selected');</script>
<style>
    #twInnerContainer {
        padding: 0;
    }

    section.full {
        width: 100%;
        padding: 60px 40px 40px;
        margin: 0;
    }

    section.full .container {
        max-width: 1180px;
        margin: 0 auto;
        background-color: transparent
    }

    section.full .container {
        max-width: 1180px;
        margin: 0 auto
    }

    .container .half {
        max-width: 500px
    }

    .profile-info label, .profile-image label {
        display: block;
        margin: 10px 0 5px;
    }

    .profile-image .face {
        width: 80px;
        height: 80px;
    }

    .container .half h1 {
        margin-bottom: 30px
    }

    .bg-blue {
      background-color: #2F97C6;
      color: #fff
    }

    .bg-blue .button.textual {
        color: #fff;
    }
</style>
<%

    String command = pageState.getCommand();

    if ("ACCOUNT_SET".equals(command)) {
        PageSeed newTask = new PageSeed(request.getContextPath() + "/applications/teamwork/task/taskNew.jsp");
        newTask.addClientEntry("ADD_TYPE", "ADD_TASK");
        newTask.setCommand(Commands.ADD);

        ButtonSupport bl = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_TASK"), 720, 800, newTask);
        bl.enabled = loggedOp.hasPermissionFor(TeamworkPermissions.project_canCreate);
%>
<div class="mainColumn" style="width: 100%; padding:0">
    <section class="full bg-blue">
        <div class="container">
            <div class="half">
                <h1><%=I18n.get("TW_SUGG_01", pageState.getLoggedOperator().getDisplayName())%>
                </h1>
                <h3><%=I18n.get("TW_SUGG_03")%></h3>
            </div>
        </div>
    </section>
    <section class="full" style="padding-top: 20px">
        <div class="container">
            <div>
                <p><img src="../../img/homeSample.png"
                        style="float: right; margin-top: -300px; width: 50%; height: auto; margin-left:50px "><br>
                    <%
                        bl.outputModality = "GRAPHICAL";
                        bl.label = I18n.get("TW_SUGG_04");
                        bl.additionalCssClass = "first xl";
                        bl.toHtml(pageContext);
                    %>
                </p>
            </div>
            <p style="text-align: center">
                <br>
            </p>
        </div>
    </section>
</div>
<%

} else {
    PageSeed seed = pageState.thisPage(request);
    Form form = new Form(seed);
    pageState.setForm(form);
    form.alertOnChange = true;
    form.encType = Form.MULTIPART_FORM_DATA;
    form.start(pageContext);

    //MAKE
    pageState.addClientEntry(OperatorConstants.FLD_SURNAME, person.getPersonSurname());
    pageState.addClientEntry(OperatorConstants.FLD_NAME, person.getPersonName());
    pageState.addClientEntry("EMAIL", person.getDefaultEmail());

    TextField tf = new TextField(I18n.get(OperatorConstants.FLD_NAME), OperatorConstants.FLD_NAME, "</td><td>", 30, false);
    tf.fieldClass = "formElements formElementsBig";

    TextField tfs = new TextField(I18n.get(OperatorConstants.FLD_SURNAME), OperatorConstants.FLD_SURNAME, "</td><td>", 30, false);
    tfs.fieldClass = "formElements formElementsBig";

    TextField email = TextField.getEmailInstance("EMAIL");
    email.separator= "</td><td>";
    email.fieldSize= 30;
    email.label=I18n.get("EMAIL");
    email.fieldClass = "formElements formElementsBig";

    TextField companyName = new TextField(I18n.get("COMPANY_NAME"), "COMPANY_NAME", "</td><td>", 30, false);
    companyName.fieldClass = "formElements formElementsBig";
    companyName.required = true;

%>
<%--------------------------------------------------------------------------   CONTAINER  ---------------------------------------------------------------------------------------------------------%>

<div class="mainColumn" style="width: 100%; padding:0">
    <section class="full bg-blue" style="padding-bottom: 10px">
        <div class="container">
           <h1><%=I18n.get("SET_YOUR_PROFILE")%></h1>
        </div>
    </section>

    <section class="full" style="padding-top: 20px">

        <div class="container groupRow">
            <div class="groupCell col6 profile-info">
                <p><%companyName.toHtml(pageContext);%></p>
                <p><%tf.toHtml(pageContext);%></p>
                <p><%tfs.toHtml(pageContext);%></p>
                <p><%email.toHtml(pageContext);%></p>
            </div>
            <div class="groupCell col6 offset-left profile-image">
                <jsp:include page="firstStartPart.jsp"></jsp:include>

            </div>
            <div class="groupRow">
                <%
                    ButtonBar buttonBar = new ButtonBar();
                    ButtonSubmit save = ButtonSubmit.getSaveInstance(pageState);
                    save.additionalCssClass = "big first";
                    buttonBar.addButton(save);
                    buttonBar.toHtml(pageContext);
                %>
            </div>

        </div>
    </section>
</div>
<%
    form.end(pageContext);
        }
    }
%>






