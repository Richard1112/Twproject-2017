package com.teamwork.expand;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.jblooming.logging.Auditable;
import org.jblooming.ontology.LoggableIdentifiableSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.hibernate.PersistenceContext;

public class TaskService extends LoggableIdentifiableSupport implements Comparable, Auditable{
  
	private String serviceId;
  
    private String resourceId;
    
    private String serviceTypeId;
    
    
    private Date serviceDate;
    
    private String totalNum;
    
    private String taskId;
    
    
    private Set<TaskServiceItem> taskServiceItemSet=new HashSet<TaskServiceItem>();

	public String getServiceId() {
		return serviceId;
	}





	public Set<TaskServiceItem> getTaskServiceItemSet() {
		return taskServiceItemSet;
	}





	public void setTaskServiceItemSet(Set<TaskServiceItem> taskServiceItemSet) {
		this.taskServiceItemSet = taskServiceItemSet;
	}





	public String getServiceTypeId() {
		return serviceTypeId;
	}

	public void setServiceTypeId(String serviceTypeId) {
		this.serviceTypeId = serviceTypeId;
	}

	public String getTaskId() {
		return taskId;
	}

	public void setTaskId(String taskId) {
		this.taskId = taskId;
	}

	public void setServiceId(String serviceId) {
		this.serviceId = serviceId;
	}

	public String getResourceId() {
		return resourceId;
	}

	public void setResourceId(String resourceId) {
		this.resourceId = resourceId;
	}

	public String getTotalNum() {
		return totalNum;
	}

	public void setTotalNum(String totalNum) {
		this.totalNum = totalNum;
	}

	public Date getServiceDate() {
		return serviceDate;
	}

	public void setServiceDate(Date serviceDate) {
		this.serviceDate = serviceDate;
	}
  
	public static TaskService load(Serializable mainObjectId) throws FindByPrimaryKeyException {
		    return load(mainObjectId, PersistenceContext.getDefaultPersistenceContext());
	}


	  public static TaskService load(Serializable mainObjectId, PersistenceContext pc) throws FindByPrimaryKeyException {
	    return (TaskService) PersistenceHome.findByPrimaryKey(TaskService.class, mainObjectId, pc);
	  }
	  
	public BigDecimal getSubTotalNum(){
		BigDecimal subTotalNum=BigDecimal.ZERO;
		if(CollectionUtils.isNotEmpty(taskServiceItemSet)){
			for(TaskServiceItem tsi:taskServiceItemSet){
				subTotalNum=subTotalNum.add(new BigDecimal(tsi.getTotalNum()));
			}
		}
		return subTotalNum;
	}

}
