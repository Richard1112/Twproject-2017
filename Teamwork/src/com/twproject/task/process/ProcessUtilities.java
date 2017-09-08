package com.twproject.task.process;

import com.twproject.task.Assignment;
import com.twproject.task.Task;
import com.twproject.task.TaskBricks;
import com.twproject.resource.Resource;
import com.twproject.operator.TeamworkOperator;
import com.twproject.security.RoleTeamwork;
import com.twproject.setup.WizardSupport;
import org.jblooming.oql.OqlQuery;
import org.jblooming.ontology.PerformantNode;
import org.jblooming.utilities.DateUtilities;
import org.jbpm.graph.node.TaskNode;

import java.util.Set;
import java.util.Date;

public class ProcessUtilities {

  public static Task getTaskByJbpmTaskNode(Task parentTWTask,TaskNode taskNode){
    Task task=null;

    // go up until find the processInstance
    if (parentTWTask.isProcessDriven() && parentTWTask.getTaskProcess()==null){
      return getTaskByJbpmTaskNode(parentTWTask.getParent(),taskNode);
    }

    for (PerformantNode pn:parentTWTask.getChildren()){
      if ((taskNode.getId()+"").equals ( ((Task) pn).getExternalCode())){
        task= (Task) pn;
        break;
      }
    }

    return task;

  }

  public static Assignment getAssignmentByJbpmTask(Task teamworkTask, org.jbpm.taskmgmt.def.Task jbpmTask){
    Assignment assig=null;

    // go up until find the processInstance
    if (teamworkTask.isProcessDriven() && teamworkTask.getTaskProcess()==null){
      return getAssignmentByJbpmTask(teamworkTask.getParent(),jbpmTask);
    }

    String oql="select ass from "+ Assignment.class.getName()+" as ass where ass.task.ancestorIds= :ids and ass.externalCode=:jbt";
    OqlQuery q=new OqlQuery(oql);
    q.getQuery().setParameter("ids",teamworkTask.getChildAncentorIds());
    q.getQuery().setParameter("jbt",jbpmTask.getId()+"");

    assig= (Assignment) q.getQuery().uniqueResult();

    return assig;

  }


  public static Assignment createAssignment(Task twTask, Resource res, String role, String estimated, org.jbpm.taskmgmt.def.Task jbpmTask, String description, TeamworkOperator loggedOperator, boolean createAssigEvenIfAlreadyOne) throws Exception {
    Assignment ass = twTask.getFirstAssignmentsForResource(res);
    if (ass == null) { //there is no assig for the resource

      if (!createAssigEvenIfAlreadyOne) {  // check if there is already an assig for the same task
        for (Assignment as : twTask.getAssignments()) {
          if ((jbpmTask.getId() + "").equals(as.getExternalCode())) {
            ass = as;
            break;
          }
        }
      }

      //create an assignment
      if (ass == null) {

        Task taskProcessRoot=twTask.getParent();

        //get the role
        RoleTeamwork twRole = WizardSupport.getRoleByNameAndArea(role, taskProcessRoot.getArea());
        if (twRole == null)
          twRole = TaskBricks.getWorkerRole(taskProcessRoot.getArea());

        if (twRole != null) {
          ass = new Assignment();
          ass.setTask(twTask);
          ass.setResource(res);
          ass.setOwner(loggedOperator);
          ass.setRole(twRole);

          long millis = 0;
          try {
            millis = DateUtilities.millisFromString(estimated, false);
          } catch (NumberFormatException nfe) {
          }

          ass.setEstimatedWorklog(millis);
          ass.setAssignmentDate(new Date());
          ass.setExternalCode(jbpmTask.getId() + "");

          // set description from process
          ass.setDescription(description);

          ass.store();

          //send notification message and subscribe for events
          Assignment.notifyAndSubscribe(ass,loggedOperator);


        } else {
          throw new Exception("Role not found: " + role);
        }
      }
    }
    return ass;
  }

}
