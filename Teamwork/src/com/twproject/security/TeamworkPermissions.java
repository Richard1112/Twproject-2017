package com.twproject.security;

import org.jblooming.security.Permission;
import org.jblooming.security.Permissions;

/**
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 *         Date: 8-apr-2005 : 15.46.02
 */
public class TeamworkPermissions extends Permissions {

  protected static final String TW_BASE = "TW_";

  public static final Permission system_canManageCalendar = new Permission(TW_BASE + "sys_cal");

  public static final Permission resource_canRead = new Permission(TW_BASE + "res_r");
  public static final Permission resource_canWrite = new Permission(TW_BASE + "res_w");
  public static final Permission resource_canCreate = new Permission(TW_BASE + "res_c");
  public static final Permission resource_canDelete = new Permission(TW_BASE + "res_d");

  public static final Permission resource_cost_canRead = new Permission(TW_BASE + "res_cstiu");


  @Deprecated
  public static final Permission resource_assignment_manage = new Permission(TW_BASE + "res_ass"); // used for giving assignability to all resources of an area

  @Deprecated
  public static final Permission task_assignment_manage = new Permission(TW_BASE + "tk_ass");   //same but at task level

  @Deprecated //usare resource_manage
  public static final Permission assignment_manage = new Permission(TW_BASE + "ass_man"); //dovrebbe funzionare e non dare noia


  // in fase di crazione assig consente di poter selezionare dal combo le risorse dell'area su cui è dato (task o globale)
  // e vedere operator load e gestire piano e priority di task o area. Sei il "manager" della risorsa.
  // NON da il permesso di creare/salvare/eliminare assig
  public static final Permission resource_manage = new Permission(TW_BASE + "ass_man"); // è il vecchio assignment_manage ma


  // consente la creazione/modifica/salvataggio dell'oggetto assegnazione. NON da permessi su CHI si può assegnare.
  // da usare solo sugli editor dell'assegnazione
  public static final Permission assignment_canCRW = new Permission(TW_BASE + "ass_crw");


  public static final Permission worklog_manage = new Permission(TW_BASE + "wl_man");
  public static final Permission expense_manage = new Permission(TW_BASE + "exp_man"); // permette di gestire le spese delle assegnazioni


  public static final Permission project_canCreate = new Permission(TW_BASE + "prj_c"); // solo globale. serve per creare il progetto di radice

  public static final Permission task_canCreate = new Permission(TW_BASE + "task_c"); // sia locale che globale. Permette di creare sub-task
  public static final Permission task_canDelete = new Permission(TW_BASE + "task_d");
  public static final Permission task_canWrite = new Permission(TW_BASE + "task_w");
  public static final Permission task_canRead = new Permission(TW_BASE + "task_r");
  public static final Permission task_canChangeStatus = new Permission(TW_BASE + "task_s");


  public static final Permission task_cost_canRead = new Permission(TW_BASE + "tk_cost_r");
  public static final Permission task_cost_canCreate = new Permission(TW_BASE + "tk_cost_c");
  public static final Permission task_cost_canWrite = new Permission(TW_BASE + "tk_cost_w");

  public static final Permission issue_canRead = new Permission(TW_BASE + "issue_r");
  public static final Permission issue_canWrite = new Permission(TW_BASE + "issue_w");
  public static final Permission issue_canCreate = new Permission(TW_BASE + "issue_c");
  public static final Permission issue_canDelete = new Permission(TW_BASE + "issue_d");
  public static final Permission issue_canChangeStatus = new Permission(TW_BASE + "issue_s");  // è un permesso "finto", non usato nei ruoli. E' compreso nel write. Teestato sull'hasPermissionFor della issue

  public static final Permission document_canRead = new Permission(TW_BASE + "doc_r");
  public static final Permission document_canWrite = new Permission(TW_BASE + "doc_w");
  public static final Permission document_canCreate = new Permission(TW_BASE + "doc_c");
  public static final Permission document_canDelete = new Permission(TW_BASE + "doc_d");
  
  
  
  public static final Permission report_canRead = new Permission(TW_BASE + "report_r");
  public static final Permission report_canWrite = new Permission(TW_BASE + "report_w");
  public static final Permission report_canCreate = new Permission(TW_BASE + "report_c");
  public static final Permission report_canDelete = new Permission(TW_BASE + "report_d");

  public static final Permission classificationTree_canManage = new Permission(TW_BASE + "clTree_m");

  public static final Permission board_canRead = new Permission(TW_BASE + "board_r");
  public static final Permission board_canWrite = new Permission(TW_BASE + "board_w");
  public static final Permission board_canCreate = new Permission(TW_BASE + "board_c");

  public static final Permission fileStorage_canRead =new Permission(TW_BASE + "fileStor_r");
  public static final Permission fileStorage_canWrite =new Permission(TW_BASE + "fileStor_w");
  public static final Permission fileStorage_canCreate =new Permission(TW_BASE + "fileStor_c");

  public static final Permission fileStorage_explorer_canRead =new Permission(TW_BASE + "filStoEx_r");
  public static final Permission fileStorage_explorer_canWrite =new Permission(TW_BASE + "filStoEx_w");
  public static final Permission fileStorage_explorer_canCreate =new Permission(TW_BASE + "filStoEx_c");

	public static final Permission task_audit_canCreate = new Permission(TW_BASE + "taskadt_c");
	public static final Permission task_audit_canAudit = new Permission(TW_BASE + "taskadt_a");
	public static final Permission task_audit_canRCreate = new Permission(TW_BASE + "taskadt_rc");

}
