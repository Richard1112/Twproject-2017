<%@ page
	import="com.twproject.task.Task,com.twproject.operator.TeamworkOperator, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.constants.Commands, com.twproject.resource.Person, org.jblooming.waf.view.PageState, com.twproject.resource.ResourceBricks, com.twproject.security.RoleTeamwork, com.twproject.task.TaskBricks, org.jblooming.waf.settings.ApplicationState, com.twproject.task.Assignment, com.twproject.resource.Resource, org.jblooming.utilities.JSP, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.Bricks, org.jblooming.waf.PageQuark, org.jblooming.waf.html.button.ButtonSupport, org.jblooming.waf.PluginBricks, java.util.Set, org.jblooming.waf.html.input.Uploader, com.twproject.document.TeamworkDocument, com.twproject.document.DocumentBricks, org.jblooming.waf.html.button.AHref, org.jblooming.waf.html.display.Img, com.twproject.task.TaskStatus, org.jblooming.utilities.HttpUtilities, org.jblooming.remoteFile.Document, org.jblooming.remoteFile.Document.*, org.jblooming.designer.Designer, org.jblooming.designer.DesignerData, org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.container.DivOnMouseover, com.twproject.security.TeamworkPermissions, org.jblooming.waf.html.core.JspHelper, java.util.HashSet, java.util.List, org.jblooming.security.License, org.jblooming.operator.Operator"%>
<div style="width: 20%" class="container">
	<%
		PageState pageState = PageState.getCurrentPageState(request);
		Task task = (Task) pageState.getMainObject();

		Operator logged = pageState.getLoggedOperator();

		if (!task.isNew()) {

			// ----------------------------- PROJECT IMAGE--------------------------------
			if (task.bricks.getImage() != null) {
	%>
	<div id="taskImage"
		<%=task.bricks.getImage() == null ? "" : "class=\"canEdit\""%>
		style="display:<%=task.bricks.getImage() == null ? "none" : "block"%>;"
		onclick="openTaskImageEditor('<%=task.getId()%>')">
		<div class="imageUploaderOpener">
			<span class="teamworkIcon " style="cursor: default">e</span>
		</div>
		<img src="<%=task.bricks.getImageUrl()%>" style="width: 100%;">
	</div>
	<%
		} else {
	%>
	<div id="taskImage"
		style="display:<%=task.bricks.getImage() == null ? "none" : "block"%>;"
		onclick="openTaskImageEditor('<%=task.getId()%>')"></div>

	<%
		}

			// ----------------------------- WORKGROUP START --------------------------------
	%>
	<%--<div class="separator"></div>--%>
	<div class="workgroup">
		<h2><%=I18n.get("WORKGROUP")%></h2>
		<div class="clearfix">
			<%
				PageSeed psViewAssignment = pageState.pageFromRoot("task/taskAssignmentEditor.jsp");

					Person myself = ResourceBricks.myself(pageState);
					boolean iAmInvolved = false;
					boolean isThereAPM = false;

					Set<Resource> visitedRes = new HashSet();
					try {
						RoleTeamwork pmRole = TaskBricks.getProjectManagerRole(task.getArea());
						RoleTeamwork wRole = TaskBricks.getWorkerRole(task.getArea());

						String pmrn = ApplicationState.getApplicationSetting("DEFAULT_PROJECT_MANAGER_ROLE_NAME",
								"Project manager");

						for (Assignment a : task.getAssignments()) {
							isThereAPM = isThereAPM || pmrn.equalsIgnoreCase(a.getRole().getName());
							if (a.getResource().equals(myself)) {
								iAmInvolved = true;
								break;
							}
						}

						if (!iAmInvolved) {
							psViewAssignment.addClientEntry("ASSIGNEE", myself.getId());
							psViewAssignment.addClientEntry("ASSIG_ROLE", pmRole);

						} else if (!isThereAPM && task.getArea() != null) {
							psViewAssignment.addClientEntry("ASSIG_ROLE", pmRole);

						} else if (task.getArea() != null) {
							psViewAssignment.addClientEntry("ASSIG_ROLE", wRole);
						}

					} catch (Throwable t) {
					}

					for (Assignment ass : task.getAssignementsSortedByRole()) {
						Resource res = ass.getResource();
						visitedRes.add(res);
						psViewAssignment.addClientEntry("TASK_ID", task.getId());
						psViewAssignment.mainObjectId = ass.getId();
						psViewAssignment.command = Commands.EDIT;
			%><div class="workgroupElement">
				<img src="<%=res.bricks.getAvatarImageUrl()%>"
					onclick="location.href='<%=psViewAssignment.toLinkToHref()%>';"
					title="<%=res.getDisplayName() + " (" + JSP.w(ass.getRole().getCode()) + ")"%>"
					style="cursor: pointer;" class="face">
			</div>
			<%
				}

					//si completa il workgroup con gli elementi dei figli  MAX 15
					int i = 1;
					Set<Resource> workGroup = task.getWorkGroup();
					for (Resource res : workGroup) {
						if (!visitedRes.contains(res)) {
							visitedRes.add(res);
			%><div class="workgroupElement">
				<img src="<%=res.bricks.getAvatarImageUrl()%>"
					title="<%=res.getDisplayName()%>" style="cursor: pointer;"
					class="face">
			</div>
			<%
				}
						if (i > 30 && workGroup.size() >= i) {
			%><a class="faceMore" title="<%=I18n.get("MORE")%>"
				href="taskAssignmentList.jsp?CM=FN&TASK_ID=<%=task.getId()%>"><span
				class="teamworkIcon"
				style="line-height: 36px; font-size: 20px; cursor: pointer">&hellip;</span></a>
			<%
				break;
						}
						i++;
					}

					if (task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)) {
			%><div id="hintFirstAssig" class="workgroupElement">
				<%
					//JOIN THIS PROJECT
							PageSeed psAddAssignment = pageState.pageFromRoot("task/taskAssignmentNew.jsp");
							psAddAssignment.addClientEntry("TASK_ID", task.getId());
							psAddAssignment.command = Commands.ADD;

							ButtonSupport jtp = ButtonLink.getBlackInstance("", 600, 800, psAddAssignment);
							jtp.toolTip = I18n.get("ADD_ASSIGNMENT");
							jtp.iconChar = "r";
							jtp.toHtml(pageContext);
				%>
			</div>
			<%
				}
			%>
		</div>
		<%
			if (JSP.ex(workGroup)) {
		%><div class="buttonBoxInline">
			<h5><%=I18n.get("ACTIONS_FOR_THIS_WORKGROUP")%></h5>
			<%
				{
							PageSeed aw = pageState.pageFromRoot("agenda/agendaWeekDay.jsp");
							Bricks.addReferral(task.getId(), Task.class, aw);
							aw.setCommand("FIND_BY_ENTITY");
							ButtonLink buttonLink = new ButtonLink(I18n.get("AGENDA_MEETING"), aw);
							buttonLink.toolTip = I18n.get("AGENDA_MEETING");
							buttonLink.id = "btnAgenda";
							buttonLink.additionalCssClass = "small";
							buttonLink.iconChar = "m";
							buttonLink.toHtmlInTextOnlyModality(pageContext);
						}
						{
							PageSeed pssm = pageState.pageFromRoot("messaging/composeMessage.jsp");
							Bricks.addReferral(task.getId(), Task.class, pssm);
							pssm.setCommand("FIND_BY_ENTITY");
							pssm.setPopup(true);
							ButtonSupport buttonLink = ButtonLink.getBlackInstance(I18n.get("TASK_WORKGROUP_SEND_MESSAGE"),
									600, 800, pssm);
							buttonLink.additionalCssClass = "small";
							buttonLink.iconChar = "&#xec;";
							buttonLink.toHtmlInTextOnlyModality(pageContext);
						}

						{
							PageSeed ol = pageState.pageFromRoot("task/worklog/worklogOverview.jsp");
							ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
							Bricks.addReferral(task.getId(), Task.class, ol);
							ol.setCommand("FIND_BY_ENTITY");
							ButtonLink buttonLink = new ButtonLink(I18n.get("TIMESHETS_OVERVIEW"), ol);
							buttonLink.toolTip = I18n.get("TIMESHETS_OVERVIEW");
							buttonLink.additionalCssClass = "small";
							buttonLink.iconChar = "A";
							buttonLink.toHtmlInTextOnlyModality(pageContext);
						}
			%><br>
			<h5><%=I18n.get("PLAN_MENU")%></h5>
			<%
				{
							PageSeed pl = pageState.pageFromRoot("task/plan/planByTask.jsp");
							pl.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
							pl.mainObjectId = task.getId();
							pl.command = Commands.EDIT;
							ButtonLink buttonLink = new ButtonLink(I18n.get("PLAN_BY_RESOURCE"), pl);
							buttonLink.toolTip = I18n.get("PLAN_BY_RESOURCE");
							buttonLink.additionalCssClass = "small lreq30 lreqLabel lreqActive";
							buttonLink.toHtmlInTextOnlyModality(pageContext);
						}
						{
							PageSeed ol = pageState.pageFromRoot("task/plan/operatorLoad.jsp");
							ol.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
							Bricks.addReferral(task.getId(), Task.class, ol);
							ol.setCommand("FIND_BY_ENTITY");
							ButtonLink buttonLink = new ButtonLink(I18n.get("TASK_WORKGROUP_OPERATOR_LOAD"), ol);
							buttonLink.toolTip = I18n.get("TASK_WORKGROUP_OPERATOR_LOAD");
							buttonLink.additionalCssClass = "small lreq20 lreqLabel lreqActive";
							buttonLink.toHtmlInTextOnlyModality(pageContext);
						}

						if (task.hasPermissionFor(logged, TeamworkPermissions.assignment_canCRW)
								|| task.hasPermissionFor(logged, TeamworkPermissions.resource_manage)) {
							PageSeed ap = pageState.pageFromRoot("task/assignment/assignmentPriority.jsp");
							ap.addClientEntry(pageState.getEntry("FOCUS_MILLIS"));
							Bricks.addReferral(task.getId(), Task.class, ap);
							ap.setCommand("FIND_BY_ENTITY");
							ButtonLink buttonLink = new ButtonLink(I18n.get("TASK_WORKGROUP_PRIORITY"), ap);
							buttonLink.toolTip = I18n.get("TASK_WORKGROUP_PRIORITY");
							buttonLink.additionalCssClass = "small lreq20 lreqLabel lreqActive";
							buttonLink.toHtmlInTextOnlyModality(pageContext);
						}
			%>
		</div>
		<%
			}
		%>
	</div>
	<div class="separator"></div>

	<%
		PageSeed psAddAssignment = pageState.pageFromRoot("task/taskCustomerFieldEdit.jsp");
			psAddAssignment.addClientEntry("TASK_ID", task.getId());
			psAddAssignment.command = Commands.ADD;

			ButtonSupport jtp = ButtonLink.getBlackInstance("", 600, 800, psAddAssignment);
			jtp.toolTip = "Edit Customer Field";
			jtp.label = "Edit Customer Field";
			jtp.iconChar = "e";
			jtp.additionalCssClass = "small block";
			jtp.toHtml(pageContext);
	%>
<br />
	<%
		TeamworkOperator loggedp = (TeamworkOperator) pageState.getLoggedOperator();
			Resource loggedRes = loggedp.getPerson();
			if (task.bricks.hasAudit(loggedRes.getId().toString(), task.getId().toString()) != null) {
				PageSeed psAddAssignmentAudit = pageState.pageFromRoot("task/taskAuditList.jsp");
				psAddAssignmentAudit.addClientEntry("TASK_ID", task.getId());
				psAddAssignmentAudit.command = Commands.AUDIT;

				ButtonSupport jtpAudit = ButtonLink.getBlackInstance("", 600, 900, psAddAssignmentAudit);
				jtpAudit.toolTip = I18n.get("TASK_AUDIT");
				jtpAudit.label = I18n.get("TASK_AUDIT");
				jtpAudit.iconChar = "e";
				jtpAudit.enabled = task.hasPermissionFor(logged, TeamworkPermissions.task_audit_canAudit);
				jtpAudit.additionalCssClass = "small block delete";
				jtpAudit.toHtml(pageContext);
	%><br />
	<%
		}
	%>
	<%
		PageSeed psAddAssignment2 = pageState.pageFromRoot("task/taskAuditList.jsp");
			psAddAssignment2.addClientEntry("TASK_ID", task.getId());
			psAddAssignment2.addClientEntry("ISCLOSED", "1");
			psAddAssignment2.command = Commands.S_AUDIT;

			ButtonSupport jtp2 = ButtonLink.getBlackInstance("", 600, 900, psAddAssignment2);
			jtp2.toolTip = I18n.get("SUBMIT_AUDIT");
			jtp2.label = I18n.get("SUBMIT_AUDIT");
			jtp2.iconChar = "e";
			jtp2.additionalCssClass = "small block";
			jtp2.enabled = task.hasPermissionFor(logged, TeamworkPermissions.task_audit_canCreate);
			jtp2.toHtml(pageContext);
	%><br />
	<%
    if(task.getParent()==null){
    	 PageSeed psAddAssignment3 = pageState.pageFromRoot("task/service/taskSupplierService.jsp");
    	 psAddAssignment3.addClientEntry("TASK_ID",task.getId());
    	 psAddAssignment3.command = Commands.EDIT;
   	    ButtonSupport jtp3 = ButtonLink.getBlackInstance("",700,900, psAddAssignment3);
   	    jtp3.toolTip=I18n.get("SUPPLIER_SERVICE");
   	    jtp3.label=I18n.get("SUPPLIER_SERVICE");
   	    jtp3.iconChar="e";
   	    jtp3.additionalCssClass = "small block";
   	    jtp3.toHtml(pageContext);
    }
%>
	<%
		// ----------------------------- DOCUMENTS START --------------------------------

			if (true || pageState.href.indexOf("taskDocument") <= 0) {

				PageSeed psDocumentList = pageState.pageFromRoot("task/taskDocumentList.jsp");
				psDocumentList.setCommand("LIST_DOCS");
				psDocumentList.addClientEntry("TASK_ID", task.getId());
				ButtonLink blDocumentList = new ButtonLink(I18n.get("DOCUMENTS"), psDocumentList);
				blDocumentList.enabled = !task.isNew()
						&& task.hasPermissionFor(logged, TeamworkPermissions.document_canRead);
				blDocumentList.hasFocus = pageState.getHref().indexOf("Document") >= 0;
	%>
	<div class="separator"></div>
	<div class="documents">
		<h2>
			<span <%=blDocumentList.generateLaunchJs()%>
				style="cursor: pointer; display: inline"><%=I18n.get("DOCUMENTS")%></span>
			<%
				if (task.hasPermissionFor(logged, TeamworkPermissions.document_canCreate)) {

							//JspHelper addButton = null;

							ButtonJS addOpener = new ButtonJS("");
							addOpener.toolTip = I18n.get("ADD");
							addOpener.label = "";
							addOpener.additionalCssClass = "edit";
							addOpener.iconChar = "P";
							DivOnMouseover add = new DivOnMouseover(addOpener);

							//addButton = add;

							// ADD DOCUMENT UPLOAD
							PageSeed ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
							ps.setCommand(Commands.ADD);
							ps.addClientEntry("TASK_ID", task.getId());
							ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_UPLOAD);
							ButtonSupport addDoc = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_DOCUMENT_UPLOAD"), 700,
									1000, ps);
							add.addButton(addDoc);

							// ADD DOCUMENT FILE STORAGE solo se hai permessi globali
							ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
							ps.setCommand(Commands.ADD);
							ps.addClientEntry("TASK_ID", task.getId());
							ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_FILE_STORAGE);
							addDoc = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_DOCUMENT_REMOTE"), 700, 1000, ps);
							addDoc.enabled = logged.hasPermissionFor(TeamworkPermissions.fileStorage_canRead); // messo il bottono per far vedere che esiste questa possibilità
							if (License.assertLevel(30))
								add.addButton(addDoc);
							//}

							// ADD DOCUMENT URL
							ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
							ps.setCommand(Commands.ADD);
							ps.addClientEntry("TASK_ID", task.getId());
							ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_URL);
							addDoc = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_DOCUMENT_URL"), 600, 700, ps);
							add.addButton(addDoc);

							// ADD CONTENT
							ps = pageState.pageFromRoot("task/taskDocumentEditor.jsp");
							ps.setCommand(Commands.ADD);
							ps.addClientEntry("TASK_ID", task.getId());
							ps.addClientEntry("DOCUMENT_TYPE", TeamworkDocument.IS_CONTENT);
							addDoc = ButtonLink.getBlackInstance("+ " + I18n.get("ADD_DOCUMENT_CONTENT"), 700, 700, ps);
							add.addButton(addDoc);
			%><span style="float: right; font-size: 14px; padding-top: 7px;">
				<%
					add.toHtmlInTextOnlyModality(pageContext);
				%>
			</span>
			<%
				}
			%>
		</h2>
		<div class="customFormsList">
			<%
				// ----------------------------- CUSTOM FORMS  --------------------------------
						boolean visibleForm = false;
						Set<PageQuark> pqs = PluginBricks.getPageQuarkGroup("TASK_FORMS");
						for (PageQuark pq : pqs) {
							if (pq.isVisibleInThisContext(pageState)) {
								visibleForm = true;
								break;
							}
						}

						if (visibleForm) {
							PageSeed edf = pageState.pageFromRoot("document/drawModule.jsp");
							edf.mainObjectId = task.getId();
							for (PageQuark pq : pqs) {
								if (pq.isVisibleInThisContext(pageState)) {
									edf.addClientEntry("DESIGNER_URL_TO_INCLUDE", pq.getHrefForInclude());
									edf.addClientEntry("DESIGNER_NAME", pq.getName());
									edf.addClientEntry("CLAZZ", Task.class.getName());

									ButtonSupport toForm = ButtonLink.getBlackInstance(I18n.get(pq.getName()), 819, 1152,
											edf);

									Designer designer = new Designer(pq.urlToInclude, pq.getName(), Task.class,
											task.getId());
									DesignerData designerData = designer.getDesignerData();
									boolean fulfilled = designerData.getValueMap().size() > 0;

									toForm.iconChar = "&#xe0;";
									toForm.additionalCssClass = "small block";
			%><div class="customFormLink lreq30 lreqLabel">
				<%
					toForm.toHtmlInTextOnlyModality(pageContext);
				%>
				<%
					if (fulfilled) {
				%><span class="teamworkIcon"
					style="position: absolute; right: 0; top: 0; font-size: 120%; cursor: default"
					title="<%=I18n.get("FORM_FILLED")%>">&#x2039;</span>
				<%
					}
				%>
			</div>
			<%
				}
							}

						}
			%>
		</div>
		<%
			// document drop area when not in document list
					if (pageState.href.indexOf("taskDocument") < 0
							&& task.hasPermissionFor(logged, TeamworkPermissions.document_canCreate)) {
		%>
		<div id="holder" class="uploadizeDrop" style="display: none"></div>
		<script>
    $(function(){
      $("#holder").uploadize({
        url:"taskAjaxController.jsp",
        maxSize:<%=Uploader.getMaxUploadSizeInByte()%>,
        additionalRequestParameters:{CM:"DROPDOC",OBJID:<%=task.getId()%>},
        onLoadCallback:function(response){
          if (response.ok){

            var fileWrapper = $("<div/>").addClass("docLabelWrapper");
            var dr = $(response.docHRef);
            var image = $("<img>"); //response.docMime;
            image.attr({src:contextPath+"/img/mime/" + response.docMime}).css({width:20});
            fileWrapper.append(image).append(dr);
            $("#documentList").append(fileWrapper);
            dr.effect("highlight", { color: "#F9EFC5" }, 2500);
//            self.location.reload();
          }
        },
        fileAreaSelector:"#documentList"
      });
    });
  </script>
		<%
			}
		%><div id="documentList" class="textSmall">
			<%
				for (TeamworkDocument document : DocumentBricks.getLastVersionDocuments(task)) {
							AHref aHref = document.bricks.getContentLink(pageState);
			%><div class="docLabelWrapper">
				<%
					document.bricks.getMimeImage().toHtml(pageContext);
								aHref.toHtml(pageContext);
				%>
			</div>
			<%
				}
			%>
		</div>








	</div>
	<%
		}
		}
	%>
	<script type="text/javascript">
  function warnOnClosing() {
    if ("<%=TaskStatus.STATUS_DONE%>" == $("#STATUS").val()) {
      var endSt = $("#END").val();
      if (endSt) {
        var end = Date.parseString(endSt);
        //console.debug("warnOnClosing",end);
        if (!end.equalsIgnoreTime(new Date())) {
          showFeedbackMessage("WARNING", "<%=I18n.get("TASK_CLOSING_IN_DIFFERENT_DATE")%>");
						var span = $("<span>")
								.addClass("warning textSmall")
								.append("<%=I18n.get("TASK_CLOSING_IN_DIFFERENT_DATE")%>");
          $("#LOG_STATUS_CHANGE_CONTAINER textarea").after(span);
        }
      }
    }
  }
</script>
</div>
