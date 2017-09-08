package com.twproject.task.process;

import org.jblooming.ontology.IdentifiableSupport;
import org.jblooming.operator.Operator;
import org.jbpm.graph.exe.ProcessInstance;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.ForeignKey;
import org.hibernate.annotations.Index;

import javax.persistence.*;
import java.io.Serializable;

import com.twproject.task.Task;

@Entity
@Table(name = "twk_taskprocess")
public class TaskProcess extends IdentifiableSupport {


  private Task task;
  private ProcessInstance processInstance;



  @Id
  @Type(type = "int")
  @GeneratedValue(strategy = GenerationType.AUTO)
  public Serializable getId() {
      return super.getId();
  }


  @ManyToOne(targetEntity = Task.class)
  @ForeignKey(name = "fk_tp_task")
  @Index(name = "idx_tp_task")
  public Task getTask() {
    return task;
  }

  public void setTask(Task task) {
    this.task = task;
  }

  @ManyToOne(targetEntity = ProcessInstance.class)
  @ForeignKey(name = "fk_tp_process")
  @Index(name = "idx_tp_process")
  public ProcessInstance getProcessInstance() {
    return processInstance;
  }

  public void setProcessInstance(ProcessInstance processInstance) {
    this.processInstance = processInstance;
  }


}
