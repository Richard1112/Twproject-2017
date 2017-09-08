package org.jblooming.flowork;

import org.jblooming.persistence.hibernate.*;
import org.hibernate.cfg.*;

import java.net.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class FlowHibernateConfiguration {


  public static void configure(Configuration hibConfiguration) {

    //hibConfiguration.setProperty(Environment.HBM2DDL_AUTO, "verify");

    URL ce = HibernateFactory.class.getClassLoader().getResource("hibernate.queries.hbm.xml");
    hibConfiguration.addURL(ce);

    ce = HibernateFactory.class.getClassLoader().getResource("ProcessDefinition.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Node.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Transition.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Event.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Action.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("SuperState.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ExceptionHandler.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Delegation.hbm.xml");
    hibConfiguration.addURL(ce);

    // graph.node mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("StartState.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("EndState.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ProcessState.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Decision.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Fork.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Join.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("State.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TaskNode.hbm.xml");
    hibConfiguration.addURL(ce);

    //HOME MADE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ce = HibernateFactory.class.getClassLoader().getResource("MilestoneNode.hbm.xml");
    hibConfiguration.addURL(ce);

    //HOME MADE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ce = HibernateFactory.class.getClassLoader().getResource("MilestoneInstance.hbm.xml");
    hibConfiguration.addURL(ce);

    // graph.action mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("Script.hbm.xml");
    hibConfiguration.addURL(ce);

    // context.def mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("ContextDefinition.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("VariableAccess.hbm.xml");
    hibConfiguration.addURL(ce);

    // taskmgmt.def mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("TaskMgmtDefinition.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Swimlane.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Task.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TaskController.hbm.xml");
    hibConfiguration.addURL(ce);

    // bytes mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("ByteArray.hbm.xml");
    hibConfiguration.addURL(ce);

    // file.def mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("FileDefinition.hbm.xml");
    hibConfiguration.addURL(ce);

    // scheduler.def mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("CreateTimerAction.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("CancelTimerAction.hbm.xml");
    hibConfiguration.addURL(ce);

    // graph.exe mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("Comment.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ProcessInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("Token.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("RuntimeAction.hbm.xml");
    hibConfiguration.addURL(ce);

    // module.exe mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("ModuleInstance.hbm.xml");
    hibConfiguration.addURL(ce);

    // context.exe mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("ContextInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TokenVariableMap.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("VariableInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ByteArrayInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("DateInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("DoubleInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("HibernateLongInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("HibernateStringInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("LongInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("StringInstance.hbm.xml");
    hibConfiguration.addURL(ce);

    // taskmgmt.exe mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("TaskMgmtInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TaskInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("PooledActor.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("SwimlaneInstance.hbm.xml");
    hibConfiguration.addURL(ce);

    // scheduler.exe mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("Timer.hbm.xml");
    hibConfiguration.addURL(ce);

    // logging mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("ProcessLog.hbm.xml");
    hibConfiguration.addURL(ce);

    ce = HibernateFactory.class.getClassLoader().getResource("CompositeLog.hbm.xml");
    hibConfiguration.addURL(ce);

    ce = HibernateFactory.class.getClassLoader().getResource("ActionLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("NodeLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ProcessInstanceCreateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ProcessInstanceEndLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("SignalLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TokenCreateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TokenEndLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TransitionLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("VariableLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("VariableCreateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("VariableDeleteLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("VariableUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ByteArrayUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("DateUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("DoubleUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("HibernateLongUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("HibernateStringUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("LongUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("StringUpdateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TaskLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TaskCreateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TaskAssignLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("TaskEndLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("SwimlaneLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("SwimlaneCreateLog.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("SwimlaneAssignLog.hbm.xml");
    hibConfiguration.addURL(ce);

    // module.def mapping files
    ce = HibernateFactory.class.getClassLoader().getResource("ModuleDefinition.hbm.xml");
    hibConfiguration.addURL(ce);    

    ce = HibernateFactory.class.getClassLoader().getResource("ExecuteActionJob.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ExecuteNodeJob.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("NullInstance.hbm.xml");
    hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("ProcessStateLog.hbm.xml");
    hibConfiguration.addURL(ce);

    ce = HibernateFactory.class.getClassLoader().getResource("JcrNodeInstance.hbm.xml");
        hibConfiguration.addURL(ce);
        ce = HibernateFactory.class.getClassLoader().getResource("Job.hbm.xml");
        hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("MailAction.hbm.xml");
            hibConfiguration.addURL(ce);
    ce = HibernateFactory.class.getClassLoader().getResource("MailNode.hbm.xml");
            hibConfiguration.addURL(ce);


  }
}
