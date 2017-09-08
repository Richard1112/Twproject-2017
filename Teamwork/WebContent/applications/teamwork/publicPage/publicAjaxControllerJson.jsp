<%@ page import="com.twproject.agenda.Event, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person, com.twproject.resource.Resource, com.twproject.security.TeamworkPermissions, com.twproject.task.*, com.twproject.task.businessLogic.IssueAction, net.sf.json.JSONArray, net.sf.json.JSONObject, org.jblooming.agenda.Period, org.jblooming.messaging.MessagingSystem, org.jblooming.messaging.SomethingHappened, org.jblooming.ontology.Pair, org.jblooming.ontology.PersistentFile, org.jblooming.ontology.businessLogic.DeleteHelper, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.persistence.exceptions.PersistenceException, org.jblooming.persistence.hibernate.PersistenceContext, org.jblooming.tracer.Tracer, org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.utilities.StringUtilities, org.jblooming.waf.JSONHelper, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.exceptions.ActionException, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.input.Uploader, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.*, java.util.*" %><%@page pageEncoding="UTF-8"%>
<%
  JSONHelper jsonHelper = new JSONHelper();
  JSONObject json = jsonHelper.json;
  PageState pageState = PageState.getCurrentPageState(request);
  try {

    //si controlla subito se la chiave è buona se non lo è si esce subito zitti-zitti
    String key = pageState.getEntry("key").stringValue();
    String requesterEmail = StringUtilities.generateEmailFromKey(key);
    if (!JSP.ex(requesterEmail)) {
      return;
    }


    //---------------------------------------------------------------------- SAVE HISTORY NOTES
    if ("SVHISTNOTES".equals(pageState.command)) {
      String notes = pageState.getEntry("notes").stringValueNullIfEmpty();

      //se la issue non è arrivata o non è dell'utente estratto dalla key si esce
      Issue issue = Issue.load(pageState.getEntry("issueId").intValueNoErrorCodeNoExc() + "");
      if (issue == null || !requesterEmail.equals(issue.getExtRequesterEmail()))
        return;

      if (JSP.ex(notes)) {
        IssueHistory history = issue.addComment(notes);
        history.setExtRequesterEmail(requesterEmail);
        history.setCreator(requesterEmail);
        history.setLastModifier(requesterEmail);
        history.store();

        //notify all previous commenters
        Set<TeamworkOperator> ops = new HashSet();
        for (IssueHistory comment : issue.getComments()) {
          if (comment.getOwner()!=null)
            ops.add((TeamworkOperator) comment.getOwner());
        }

        // si inviano messaggi a tutti i commentatori precedenti
        for (TeamworkOperator op : ops) {
          issue.generateCommentMessage(null, op, history, MessagingSystem.Media.LOG);
        }

        //si genera un evento su task se si può
        issue.riseCommentAddedEvent(null, notes);


        if (history != null)
          json.element("history", history.jsonify());
      }


      //---------------------------------------------------------------------- PUBLIC DELETE HISTORY NOTES
    } else if ("DLHISTNOTES".equals(pageState.command)) {
      IssueHistory history = IssueHistory.load(pageState.getEntry("histId").intValueNoErrorCodeNoExc());

      //la puoi cancellare solo se è tua ed è l'ultima della lista ed è una nota non una history change
      if (history != null && history.getExtRequesterEmail().equals(requesterEmail)){
        List<IssueHistory> issueHistories = history.getIssue().getIssueHistories();
        boolean isTheLastOne = issueHistories.indexOf(history)==issueHistories.size()-1;
        if (isTheLastOne)
          //history.remove();
          DeleteHelper.cmdDelete(history,pageState);
      }
    }
  } catch (Throwable t) {
    jsonHelper.error(t);
  }
  jsonHelper.close(pageContext);


%>