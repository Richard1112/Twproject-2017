package com.teamwork.expand;

import java.util.Date;

import org.jblooming.logging.Auditable;
import org.jblooming.ontology.LoggableIdentifiableSupport;

public class TaskServiceItem extends LoggableIdentifiableSupport implements Comparable, Auditable{
  
	private String taskServiceId;
  
    private String serviceTypeId;
   
    private String serviceId;
    
    private String description;
    
    private String totalNum;
    private String taskId;
    private Date serviceDate;

	public String getTaskServiceId() {
		return taskServiceId;
	}

	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	public void setTaskServiceId(String taskServiceId) {
		this.taskServiceId = taskServiceId;
	}

	public String getServiceTypeId() {
		return serviceTypeId;
	}

	public void setServiceTypeId(String serviceTypeId) {
		this.serviceTypeId = serviceTypeId;
	}

	public String getServiceId() {
		return serviceId;
	}

	public void setServiceId(String serviceId) {
		this.serviceId = serviceId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Date getServiceDate() {
		return serviceDate;
	}

	public void setServiceDate(Date serviceDate) {
		this.serviceDate = serviceDate;
	}

	public String getTotalNum() {
		return totalNum;
	}

	public void setTotalNum(String totalNum) {
		this.totalNum = totalNum;
	}
    
    


  
  
}
