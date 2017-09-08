package com.twproject.document.businessLogic;

import com.twproject.document.TeamworkDocument;
import com.twproject.operator.TeamworkOperator;
import com.twproject.task.Task;
import org.jblooming.designer.DesignerData;
import org.jblooming.designer.businessLogic.DesignerController;
import org.jblooming.designer.Designer;
import org.jblooming.messaging.SomethingHappened;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jblooming.waf.exceptions.ActionException;
import org.jblooming.waf.constants.Commands;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.ApplicationException;
import org.jblooming.ontology.IdentifiableSupport;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class TeamworkDesignerController extends DesignerController{
  public TeamworkDesignerController(Designer designer) {
    super(designer);
  }

  public PageState perform(HttpServletRequest request, HttpServletResponse response)   throws PersistenceException, ActionException, org.jblooming.security.SecurityException, ApplicationException {
    PageState pageState = PageState.getCurrentPageState(request);
    boolean isNew=designer.getDesignerData().isNew(); //per scoprire se è la prima compilazione
    super.perform(request, response);

    String command = pageState.getCommand();

    if (Commands.SAVE.equals(command)) {
      if(pageState.getClientEntries().validEntries()) {
        //store the referral object in order to full index
        IdentifiableSupport reference = (IdentifiableSupport) PersistenceHome.findByPrimaryKey(designer.referenceClass, designer.referenceId);
        if (reference!=null) {
          reference.store();

          //si notifica la compilazione di un nuovo modulo per i task
          if (isNew && reference instanceof Task){
            generateCustomFormCompiledEvent((Task) reference,designer.getDesignerData(),pageState.getLoggedOperator());
          }

        }
      }
    }
    return pageState;
  }



  public static void generateCustomFormCompiledEvent(Task task,DesignerData designerData, Operator logged) throws StoreException {
    if (task != null) {
      SomethingHappened change = new SomethingHappened();
      change.setIdentifiable(task);
      change.setEventType(Task.Event.TASK_DOCUMENT_ADDED + "");

      change.setMessageTemplate("TASK_FORM_FILLED_MESSAGE_TEMPLATE");

      change.getMessageParams().put("SUBJECT_REPLACEMENT", "FORM_FILLED");
      change.getMessageParams().put("SUBJECT", I18n.get(designerData.getDesignerName()));
      change.getMessageParams().put("task", task.getDisplayName());
      change.getMessageParams().put("documentTitle",  I18n.get(designerData.getDesignerName()));
      change.setWhoCausedTheEvent(logged);


      PageSeed ps = new PageSeed(ApplicationState.serverURL + "/applications/teamwork/task/taskDocumentList.jsp");
      ps.setCommand("LIST_DOCS");
      ps.addClientEntry("TASK_ID", task.getId());
      ps.addClientEntry("DESDATA_ID", designerData.getId());

      ButtonLink edit = new ButtonLink(ps);
      edit.label = I18n.get(designerData.getDesignerName());
      change.setLink(edit.toPlainLink());

      change.store();
    }

  }

}
