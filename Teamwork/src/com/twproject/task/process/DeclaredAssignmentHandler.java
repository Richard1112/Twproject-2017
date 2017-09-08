package com.twproject.task.process;

import org.jbpm.graph.exe.ExecutionContext;
import org.jbpm.graph.node.TaskNode;
import org.jbpm.taskmgmt.exe.Assignable;
import org.jbpm.taskmgmt.def.Task;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.PlatformRuntimeException;
import com.twproject.resource.Resource;
import com.twproject.task.Assignment;

import java.util.Set;

public class DeclaredAssignmentHandler extends DinamicAssignmentHandler {

  String resource;

  public Assignment getTeamworkAssignment(Assignable assignable, ExecutionContext executionContext) {

    Assignment ass = null;

    try {

      //if "resource" param is specified
      if (JSP.ex(resource)) {
        Set<String> ress = StringUtilities.splitToSet(resource, ",");  //supports multiple resources comma delimited

        for (String res : ress) {

          //recover resource
          Resource twResource = Resource.loadByRef(res);
          if (twResource != null) {
            ass = ProcessUtilities.createAssignment(taskOnStep, twResource,role, estimated, executionContext.getTaskInstance().getTask(), JSP.w(node.getDescription()), loggedOperator, true);


          } else {
            throw new PlatformRuntimeException("Resource not found: " + resource + " in process " + executionContext.getProcessDefinition().getName() + " instance " + executionContext.getProcessInstance().getId());
          }

        }
      }

    } catch (Exception e) {
      throw new PlatformRuntimeException("Exception on " + executionContext.getProcessDefinition().getName() + " instance " + executionContext.getProcessInstance().getId(), e);

    }

    return ass;
  }

}
