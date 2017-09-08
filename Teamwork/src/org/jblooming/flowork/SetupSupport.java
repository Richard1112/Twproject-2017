package org.jblooming.flowork;

//import org.jblooming.persistence.hibernate.HibernatePropertySchemaUpdater;
import org.jbpm.context.exe.TokenVariableMap;
import org.jbpm.context.exe.VariableInstance;
import org.jbpm.graph.def.Action;
import org.jbpm.graph.def.Node;
import org.jbpm.graph.exe.ProcessInstance;
import org.jbpm.graph.exe.Token;
import org.jbpm.graph.node.TaskNode;
import org.jbpm.module.exe.ModuleInstance;
import org.jbpm.taskmgmt.def.Task;
import org.jbpm.taskmgmt.exe.PooledActor;
import org.jbpm.taskmgmt.exe.SwimlaneInstance;
import org.jbpm.taskmgmt.exe.TaskInstance;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Aug 24, 2007
 * Time: 9:51:03 AM
 */
public class SetupSupport {

  /*public static HibernatePropertySchemaUpdater upgradeSchemaToJBPM321() {
    HibernatePropertySchemaUpdater hpsu = new HibernatePropertySchemaUpdater();

    hpsu.addPropertyToCheck(Action.class, "isAsync_", false);

    hpsu.addPropertyToCheck(Node.class, "isAsync_", false);
    hpsu.addPropertyToCheck(Node.class, "ISASYNCEXCL_", false);

    hpsu.addPropertyToCheck(ProcessInstance.class, "isSuspended_", false);
    hpsu.addPropertyToCheck(ProcessInstance.class, "version_", 0);

    hpsu.addPropertyToCheck(Token.class, "version_", 0);
    hpsu.addPropertyToCheck(Token.class, "isSuspended_", false);

    hpsu.addPropertyToCheck(SwimlaneInstance.class, "version_", 0);

    hpsu.addPropertyToCheck(PooledActor.class, "version_", 0);

    hpsu.addPropertyToCheck(ModuleInstance.class, "version_", 0);

    hpsu.addPropertyToCheck(TokenVariableMap.class, "version_", 0);

    hpsu.addPropertyToCheck(VariableInstance.class, "version_", 0);

    

    hpsu.addPropertyToCheck(Task.class, "isSignalling_", true);
    hpsu.addPropertyToCheck(Task.class, "priority_", 1);

    hpsu.addPropertyToCheck(TaskNode.class, "endTasks_", false);

    hpsu.addPropertyToCheck(TaskInstance.class, "isSuspended_", false);
    hpsu.addPropertyToCheck(TaskInstance.class,"version_", 0);
    HibernatePropertySchemaUpdater.PropertyWithDefault jbpm314 = hpsu.addPropertyToCheck(TaskInstance.class, "isOpen_", false);

    jbpm314.postSql.add("UPDATE flow_node SET CLASS_ = '7' where CLASS_ = 'M'");

    jbpm314.postSql.add("UPDATE flow_processdefinition SET CLASS_ = 'P'");
    jbpm314.postSql.add("UPDATE flow_taskinstance SET CLASS_ = 'T'");

    jbpm314.postSql.add("drop table flow_message cascade constraints");
    jbpm314.postSql.add("drop table flow_timer cascade constraints");

    return hpsu;

  }  */

}
