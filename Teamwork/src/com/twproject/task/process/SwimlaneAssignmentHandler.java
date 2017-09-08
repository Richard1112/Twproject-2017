package com.twproject.task.process;

import com.twproject.operator.TeamworkOperator;
import com.twproject.task.Task;
import com.twproject.task.Assignment;
import com.twproject.task.TaskBricks;
import com.twproject.resource.Person;
import org.jblooming.ontology.PerformantNodeSupport;
import org.jblooming.ontology.SerializedMap;
import org.jblooming.operator.Operator;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.tracer.Tracer;
import org.jblooming.messaging.MessagingSystem;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.ReflectionUtilities;
import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.exe.ProcessInstance;
import org.jbpm.graph.def.Node;
import org.jbpm.taskmgmt.def.AssignmentHandler;
import org.jbpm.taskmgmt.def.Swimlane;
import org.jbpm.taskmgmt.exe.Assignable;
import org.jbpm.taskmgmt.exe.SwimlaneInstance;

import java.util.Set;

public class SwimlaneAssignmentHandler extends DefaultAssignmentHandler {



  public void assign(Assignable assignable, ExecutionContext executionContext) throws Exception {
    super.assign(assignable, executionContext);

    org.jbpm.taskmgmt.def.Task jbpmTask = executionContext.getTaskInstance().getTask();

    //in this case the assignemt should be already created by createProcessPage.jsp so we have to get the assignee id
    if (taskOnStep!=null){
      boolean foundAssig=false;
      for (Assignment ass: taskOnStep.getAssignments()){
        if ((jbpmTask.getId()+"").equals(ass.getExternalCode())){
          TeamworkOperator myself = ass.getResource().getMyself();
          if (myself!=null)
            assignable.setActorId(myself.getId()+"");
          foundAssig=true;
          break;
        }
      }

      // assignment not found: generate an assignement for logged just in order to do not hang process waiting for nobody
      if (!foundAssig){

        //recover all task for this swimlane
        SwimlaneInstance swli = (SwimlaneInstance) ReflectionUtilities.getUnderlyingObjectAsObject(assignable);
        Swimlane swimlane = swli.getSwimlane();

        //create the assignment
        ProcessUtilities.createAssignment(taskOnStep, loggedOperator.getPerson(), swimlane.getName(), "0", jbpmTask, JSP.w(jbpmTask.getTaskNode().getDescription()), loggedOperator, true);


        //recover all task for this swimlane
        Set<org.jbpm.taskmgmt.def.Task> jbpmTasks = swimlane.getTasks();

        //match twTask with jbpmTasks
        for (Task twTask : taskProcessRoot.getDescendants()) {
          if (taskOnStep.equals(twTask)) //skip this one
            continue;

          //loop for for jbTaskNodes to find
          for (org.jbpm.taskmgmt.def.Task jbTask : jbpmTasks) {
            if ((jbTask.getTaskNode().getId() + "").equals(twTask.getExternalCode())) {
              //create the assignement for task found
              ProcessUtilities.createAssignment(twTask, loggedOperator.getPerson(),swimlane.getName(),"0", jbTask, JSP.w(jbTask.getTaskNode().getDescription()),loggedOperator, true);
              break;
            }
          }
        }



        assignable.setActorId(loggedOperator.getId()+"");
      }
    }

  }

}