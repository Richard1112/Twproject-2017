package com.twproject.plan;

import com.twproject.resource.Resource;
import com.twproject.security.RoleTeamwork;
import com.twproject.task.Assignment;
import com.twproject.task.Task;
import com.twproject.task.TaskStatus;
import com.twproject.worklog.WorklogPlan;
import org.jblooming.agenda.CompanyCalendar;
import org.jblooming.agenda.Period;
import org.jblooming.agenda.Schedule;
import org.jblooming.ontology.PlatformComparators;
import org.jblooming.oql.OqlQuery;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.HashTable;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.settings.I18n;

import java.io.Serializable;
import java.util.*;

public class PlanUtilities {

  //se messo a true ignora il worklog fatto e il carico è sempre quello ideale
  public boolean ignoreWorklogWhenComputingLoad=false;

  //se messo a true ignora il planned by issue, e viene considerato solo quello inserito da piano
  //public boolean ignorePlanByIssue=false;

  public PlanUtilities() {
    ignoreWorklogWhenComputingLoad = I18n.isActive("CUSTOM_FEATURE_OPLOAD_IGNORE_WORKLOG");
  }

  public ResourceLoad getTotalLoadAndWork(Resource resource, Period period) throws PersistenceException {
    return getTotalLoadAndWork(resource, period, 0, null);
  }

  /**
   * @param resource
   * @param period
   * @param simulationLoad è una carico aggiuntivo per il periodo selezionato
   * @return
   * @throws PersistenceException
   */
  public ResourceLoad getTotalLoadAndWork(Resource resource, Period period, long simulationLoad, Serializable ignoredAssigId) throws PersistenceException {
    Tracer.Profiler prof = Tracer.getProfiler("getTotalLoadAndWork");

    ResourceLoad ret = new ResourceLoad(resource);

    //use a fake period in order get also those tasks that may influence load on extremes
    Period fakePeriod = new Period(period.getStartDate().getTime() - CompanyCalendar.MILLIS_IN_3_MONTH, period.getEndDate().getTime() + CompanyCalendar.MILLIS_IN_MONTH);

    //get resource assignments
    List<Assignment> assignmentList = resource.getActiveAssignments(fakePeriod, false, false, true, false);

    //se si passa un carico da simulare si creano una assegnazione ed un task finti
    if (simulationLoad > 0) {
      //task finto
      Task fakeTask = new Task();
      fakeTask.setSchedule(period);//ruolo finto
      fakeTask.setStatus(TaskStatus.STATUS_ACTIVE);//stato finto
      fakeTask.setDuration(CompanyCalendar.getDistanceInWorkingDays(period.getStartDate(), period.getEndDate()));
      RoleTeamwork fakeRole = new RoleTeamwork();

      //assig finta
      Assignment fakeAss = new Assignment();
      fakeAss.setResource(resource);
      fakeAss.setRole(fakeRole);
      fakeAss.setTask(fakeTask);
      fakeAss.setEstimatedWorklog(simulationLoad);
      fakeAss.setId("-10"); // a fake value

      //si mette nel mezzo a quelli buoni
      assignmentList.add(fakeAss);
    }

    long start = period.getStartDate().getTime();
    long end = period.getEndDate().getTime();

    //loop for find extremes
    for (Assignment ass : assignmentList) {
      Task task = ass.getTask();

      //devo ignorare i task in stato STATUS_UNDEFINED del tutto, mentre gli aborted sono eliminati giorno per giorno in base alla data di cambio status
      if (TaskStatus.STATUS_UNDEFINED.equals(task.getStatus()))
        continue;

      // in caso di calcolo sulla riga in edit devo non considerare la riga corrente, altrimenti raddoppio i valori
      if ((ass.getId() + "").equals(ignoredAssigId + ""))
        continue;

      Schedule sched = task.getSchedule();
      if (sched != null) {
        long endTime = sched.getEndDate().getTime();
        long startTime = sched.getStartDate().getTime();
        if (endTime - startTime < CompanyCalendar.MILLIS_IN_YEAR * 5) {
          if (startTime < start)
            start = startTime;

          if (endTime > end)
            end = endTime;

          // store milestones
          if (task.isStartIsMilestone()) {
            int st = DateUtilities.dateToInt(sched.getStartDate());
            Set<Task> taskList = ret.milestones.get(st);
            if (taskList == null) {
              taskList = new HashSet();
              ret.milestones.put(st, taskList);
            }
            taskList.add(task);
          }

          if (task.isEndIsMilestone()) {
            int st = DateUtilities.dateToInt(sched.getEndDate());
            Set<Task> taskList = ret.milestones.get(st);
            if (taskList == null) {
              taskList = new HashSet();
              ret.milestones.put(st, taskList);
            }
            taskList.add(task);
          }

        }
      }
    }

    Period realPeriod = new Period(start, end);

    // create the resulting map
    TreeMap<Integer, LoadAndWork> totalLoadAndWork = new TreeMap();
    ret.totalLoadAndWork = totalLoadAndWork;

    //get resource working plan and availability
    TreeMap<Integer, Long> globalWorkPower = resource.getWorkablePlan(realPeriod);
    ret.resourceWorkPower = new TreeMap(globalWorkPower);

    List<Assignment> assSorted = new ArrayList(assignmentList);
    // sort assignments in order to put first those where the load is more "identified"
    // old :  basing on 1) first those with plan 2) then by density= estimation/task duration
    //Collections.sort(assSorted, new AssigComparatorByDensity());
    // new : basing on 1) first those with plan 2) then by density= estimation/task duration
    Collections.sort(assSorted, new AssigComparatorByDuration());
    ret.assignmentsSorted = assSorted;

    //loop all assig building the total plan from estimation and issue
    for (Assignment ass : assSorted) {
      Task task = ass.getTask();

      //devo ignorare i task in stato STATUS_UNDEFINED del tutto, mentre gli aborted sono eliminati giorno per giorno in base alla data di cambio status
      if (TaskStatus.STATUS_UNDEFINED.equals(task.getStatus()))
        continue;

      // in caso di calcolo sulla riga in edit devo non considerare la riga corrente, altrimenti raddoppio i valori
      if ((ass.getId() + "").equals(ignoredAssigId + ""))
        continue;

      Schedule sched = task.getSchedule();
      if (sched != null) {
        // ignoring task longer than 5 years
        if (sched.getEndDate().getTime() - sched.getStartDate().getTime() < CompanyCalendar.MILLIS_IN_YEAR * 5) {

          // long[0]= estimation,
          // long[1]= work done
          // long[2]= plan  -1=there is no plan for a day,  0= do not work for that day  x= you must work x hour for that day
          // long[3]= 1->plan has note 0-> no note
          Map<Integer, AssigLoadAndWork> oneAssigLines;
          if (I18n.isActive("CUSTOM_FEATURE_USE_SIMPLE_PLAN")) {
            oneAssigLines = getLoadAndWorkFast(ass, period);
          } else {
            oneAssigLines = getLoadAndWork(ass, globalWorkPower);
          }

          ret.assignementsDetailLine.put(ass, oneAssigLines);

          for (int key : oneAssigLines.keySet()) {
            LoadAndWork allAssigLine = totalLoadAndWork.get(key);
            if (allAssigLine == null) {
              allAssigLine = new LoadAndWork();
              totalLoadAndWork.put(key, allAssigLine);
            }
            AssigLoadAndWork oneAssigLineValues = oneAssigLines.get(key);
            //allAssigLine[0] += oneAssigLineValues[0] > 0 ? oneAssigLineValues[0] : 0;
            allAssigLine.workLoad += oneAssigLineValues.workLoad > 0 ? oneAssigLineValues.workLoad : 0;
            //allAssigLine[1] += oneAssigLineValues[1] > 0 ? oneAssigLineValues[1] : 0;
            allAssigLine.workDone += oneAssigLineValues.workDone > 0 ? oneAssigLineValues.workDone : 0;

              //decrement the globalWorkPower if the day is not holyday/available
              if (globalWorkPower.get(key) != null && globalWorkPower.get(key) > 0) {
              long value = globalWorkPower.get(key) - oneAssigLineValues.workLoad;
              globalWorkPower.put(key, value <= 0 ? 0 : value);
            }
          }
        }
      }
    }


    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(period.getStartDate());
    while (cc.getTimeInMillis() <= period.getValidityEndDate().getTime()) {
      int key = DateUtilities.dateToInt(cc.getTime());
      if (!totalLoadAndWork.containsKey(key))
        totalLoadAndWork.put(key, new LoadAndWork());
      cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
    }

    prof.stop();

    return ret;
  }


  /**
   * @return map with date in integer format and double array with estimated load and work done for each date in task period
   * long[0]= estimation,
   * long[1]= work done
   * long[2]= plan   -1=there is no plan for a day,  0= do not work for that day  x= you must work x hour for that day
   * long[3]= 1-> plan has note 0-> no note
   */
  private Map<Integer, AssigLoadAndWork> getLoadAndWork(Assignment assig, TreeMap<Integer, Long> globalCapacity) throws FindException {
    Tracer.Profiler prof = Tracer.getProfiler("getLoadAndWork");

    TreeMap<Integer,AssigLoadAndWork> assigLoadAndWork = new TreeMap();

    Task task = assig.getTask();
    Period taskPeriod = task.getSchedule();


    //TreeMap<Integer, Long> oa = assig.getWorklogDoneByDay(taskPeriod);
    TreeMap<Integer, Long> oa = assig.getWorklogDoneByDay();  //deve considerare tutto il wl inserito, anche quello fuori dalle date del task

    Period wlPeriod=taskPeriod;
    if (JSP.ex(oa.keySet())) {
      // si calcola il periodo per cui è stato registrato wl
      wlPeriod = new Period(DateUtilities.intToDate(oa.firstKey()), DateUtilities.intToDate(oa.lastKey()));
      wlPeriod=taskPeriod.union(wlPeriod); //si prende il periodo + largo
    }

    //fill the result map for each day with 0
    CompanyCalendar cc = new CompanyCalendar();
    cc.setTimeInMillis(wlPeriod.getValidityStartDate().getTime() - CompanyCalendar.MILLIS_IN_DAY);
    while (cc.getTimeInMillis() <= wlPeriod.getValidityEndDate().getTime() + CompanyCalendar.MILLIS_IN_DAY) {
      int key = DateUtilities.dateToInt(cc.getTime());
      //loadAndWork.put(key, new long[]{0, 0, -1, 0});
      assigLoadAndWork.put(key, new AssigLoadAndWork());
      cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
    }

    int taskStartDateInt = DateUtilities.dateToInt(taskPeriod.getStartDate());


    long doneBeforeStart=0;
    //fill work done
    for (Integer day : oa.keySet()) {
      AssigLoadAndWork retElement = assigLoadAndWork.get(day);
      retElement.workDone = oa.get(day);
      if (day<taskStartDateInt) // controllo se hai lavorato prima della data di inizio del task
        doneBeforeStart+=retElement.workDone;
    }

    //extract from resource global capacity the one matching task period in order to compute montant
    SortedMap<Integer, Long> capacity = globalCapacity.subMap(taskStartDateInt, DateUtilities.dateToInt(taskPeriod.getEndDate()) + 1);
    TreeSet<Integer> assigCapacity = new TreeSet(new PlatformComparators.InverseNumberComparator());
    assigCapacity.addAll(capacity.keySet());

    long m = 0;

    //get the planned hours for plan and issues
    Map<Integer, Assignment.PlannedWork> plan = assig.getWorklogPlan(taskPeriod, true);

    //loop to copy plan in results
    for (int key : plan.keySet()) {
      Assignment.PlannedWork planForTheDay = plan.get(key);
      //assigLoadAndWork.get(key)[2] = planForTheDay[0];
      assigLoadAndWork.get(key).loadByPlan = planForTheDay.loadByPlan;
      assigLoadAndWork.get(key).loadByIssue = planForTheDay.loadByIssue;
      //assigLoadAndWork.get(key)[3] = planForTheDay[1];
      assigLoadAndWork.get(key).hasNotes = planForTheDay.hasNotes;
    }


    //compute the plan montant and set
    m = 0;
    Map<Integer, Long> planMontant = new TreeMap();
    for (int key : assigCapacity) {
      //long pl[] = plan.get(key);
      //long plCap = pl == null ? 0 : pl[0];
      Assignment.PlannedWork pl = plan.get(key);
      long plCap = pl == null ? 0 : pl.getTotalPlanned();

      // ignore days with plan=0
      //if (plCap > 0)
      m += plCap;
      planMontant.put(key, m);
    }


    //compute the montant
    m = 0;
    Map<Integer, Long> montant = new TreeMap();
    for (int key : assigCapacity) {
      Long dayCap = capacity.get(key);
      Assignment.PlannedWork planForTheDay = plan.get(key);

      //se c'è il piano, o le issue devono vincere. Per quel giorno non puoi lavorare più di quanto previsto dal piano/issue
      //if (planForTheDay != null && planForTheDay[0] >= 0) {
      if (planForTheDay != null && (planForTheDay.loadByPlan >= 0 || planForTheDay.loadByIssue>=0)) {
        //m += 0;

        // ignore holydays  dayCap=-1
      } else if (dayCap > 0) {
        m += dayCap;
      }

      montant.put(key, m);
    }


    int today = DateUtilities.dateToInt(new Date());

    //compute estimation included issues
    double toBeDone = assig.getEstimatedWorklog();


    //deve essere decrementata di quanto eventualmentefatto prima della data di inizio
    toBeDone-=doneBeforeStart;


    // loop for all days in task
    for (int currentDay : assigLoadAndWork.keySet()) {
      AssigLoadAndWork elem = assigLoadAndWork.get(currentDay);

      //if task has been failed or closed
      String statusOn = task.getStatusOn(DateUtilities.intToDate(currentDay));
      if (!TaskStatus.STATUS_ACTIVE.equals(statusOn) && !TaskStatus.STATUS_SUSPENDED.equals(statusOn)) {
        //toBeDone = 0;
        //break;  //lo deve ignorare non saltare tutta l'assegnazione
        continue;
      }

      if (!capacity.containsKey(currentDay))
        continue;

      double currentCapacity = capacity.get(currentDay);
      double currentMontant = montant.get(currentDay);
      Assignment.PlannedWork cp = plan.get(currentDay);
      //double currentPlan= cp==null?0:cp;
      double currentPlan = cp == null ? -1 : cp.getTotalPlanned();
      Long cpm = planMontant.get(currentDay);
      double currentPlanMontant = cpm == null ? 0 : cpm;

      long currentWorkDone = elem.workDone;

//      // no work no party if not planned or worked tolto da sc e rb 5/12/12 perchè scappava qualche caso
//      if (currentMontant <=0 || (currentCapacity<=0 && currentWorkDone<=0 && currentPlan<0))
//        continue;


      //if you already worked a lot decrement the plan
      //todo controllare qui le assegnazioni SPOT: se è spot non deve entrare nell'if
      if (currentPlan > toBeDone) {
        currentPlanMontant -= currentPlan;
        currentPlan = toBeDone;
        currentPlanMontant += currentPlan;
      }

      //if the plan is bigger than reality
      //todo controllare qui le assegnazioni SPOT: se è spot non deve entrare nell'if
      if (currentPlanMontant > toBeDone) {
        currentPlanMontant = toBeDone;
      }


      double idealToday = 0;
      if (currentPlan >= 0) { // the plan win ever!
        idealToday = currentPlan;
      } else if (currentMontant != 0) {
        // ----------------------------------------------------------------- vvvv unavailabi giorni di festa sono a -1
        idealToday = ((toBeDone - currentPlanMontant) / currentMontant) * (currentCapacity < 0 ? 0 : currentCapacity);
      }

      idealToday = idealToday <= 0 ? 0 : idealToday;

      elem.workLoad = (long) idealToday;
      //elem[2] = (long) todayWorkPower;


      //hack se devo ignorare il worklog o la risorsa non è un utente reale (non ha login) si assume che si comporti nel modo ideale -> il wl per il giorno è sempre ugale a ideal
      // questo ci consente di gestire gli skill
      if (ignoreWorklogWhenComputingLoad || assig.getResource().getMyself() == null) {
        toBeDone -= idealToday;

      } else {

        if (currentDay < today)
          toBeDone -= currentWorkDone;
        else if (currentDay == today)
          toBeDone -= currentWorkDone == 0 ? idealToday : currentWorkDone;
        else
          toBeDone -= idealToday;

      }
      toBeDone = toBeDone < 0 ? 0 : toBeDone;
    }


    // if the the today status is not ACTIVE nor SUSPENDED non spalmare
    if (!TaskStatus.STATUS_ACTIVE.equals(task.getStatus()) && !TaskStatus.STATUS_SUSPENDED.equals(task.getStatus()))
      toBeDone=0;

      // if there is no place to put wl spalm it uniformely on non-holydays in the future only
    if (toBeDone > 0) {

      //first count non holydays
      int nonHoly = 0;
      for (int day : capacity.keySet()) {
        if (day >= today && capacity.get(day) > -1) {
          nonHoly++;
        }
      }


      boolean spalmEveryWhere = false;
      if (nonHoly == 0) {
        nonHoly = (int)Math.round(taskPeriod.getDurationInMillis() / (double)CompanyCalendar.MILLIS_IN_DAY);
        spalmEveryWhere = true;
      }

      //then spalm
      if (nonHoly != 0) {
        long restant = (long) toBeDone / nonHoly;
        // loop for all days in task in the future
        for (int key : capacity.keySet()) {
          if (key > today && (capacity.get(key) > -1 || spalmEveryWhere)) {
            AssigLoadAndWork elem = assigLoadAndWork.get(key);
            elem.workLoad = elem.workLoad + restant;
          }
        }
      }
    }

    prof.stop();

    return assigLoadAndWork;

  }

  /**
   * @return map with date in integer format and double array with estimated load and work done for each date in task period
   * long[0]= estimation,  FAKE
   * long[1]= work done, EMPTY
   * long[2]= plan   -1=there is no plan for a day,  0= do not work for that day  x= you must work x hour for that day
   * long[3]= 1-> plan has note 0-> no note
   */

  private Map<Integer, AssigLoadAndWork> getLoadAndWorkFast(Assignment assig, Period period) throws FindException {

    Tracer.Profiler prof = Tracer.getProfiler("getLoadAndWorkFast");

    if (assignmentsPlanCache == null) {
      preloadPlanCache(null, period);  //preload load all resources
    }


    TreeMap<Integer,AssigLoadAndWork> loadAndWork = new TreeMap();

    Task task = assig.getTask();
    Period taskPeriod = task.getSchedule();

    //fill the result map for each day with 0
    /*long dayMillis = taskPeriod.getStartDate().getTime();
    long end = taskPeriod.getEndDate().getTime();
    while (dayMillis <= end) {
      int key = DateUtilities.dateToInt(new Date(dayMillis));
      loadAndWork.put(key, new long[]{0, 0, -1, 0});
      dayMillis += CompanyCalendar.MILLIS_IN_DAY;
    }*/

    CompanyCalendar cc = new CompanyCalendar();
    cc.setTime(taskPeriod.getValidityStartDate());
    while (cc.getTimeInMillis() <= taskPeriod.getValidityEndDate().getTime()) {
      int key = DateUtilities.dateToInt(cc.getTime());
      loadAndWork.put(key, new AssigLoadAndWork());
      cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
    }


    //get the plan for plan and issues
    //Map<Integer, Long> plan = assig.getWorklogPlan(taskPeriod, false);
    Map<Integer, Assignment.PlannedWork> plan = assignmentsPlanCache.get(assig.getId() + "");


    if (plan == null)
      plan = new HashTable();

    //loop to copy plan in results
    for (int key : plan.keySet()) {
      Assignment.PlannedWork planForTheDay = plan.get(key);
      loadAndWork.get(key).loadByPlan = planForTheDay.loadByPlan;  //planned by plan
      loadAndWork.get(key).loadByIssue = planForTheDay.loadByIssue;  //planned by issue  ... non riempito
      loadAndWork.get(key).hasNotes = planForTheDay.hasNotes;  //has notes
    }

    // loop for all days in task
    for (int currentDay : loadAndWork.keySet()) {
      AssigLoadAndWork elem = loadAndWork.get(currentDay);

      Assignment.PlannedWork cp = plan.get(currentDay);
      double currentPlan = cp == null ? -1 : cp.getTotalPlanned();

      double idealToday = 0;
      idealToday = currentPlan;
      idealToday = idealToday <= 0 ? 0 : idealToday;
      elem.workLoad = (long) idealToday;
    }

    prof.stop();
    return loadAndWork;
  }

  private Map<Serializable, Map<Integer, Assignment.PlannedWork>> assignmentsPlanCache = null;

  public void preloadPlanCache(Collection<Resource> resources, Period period) throws FindException {
    Tracer.Profiler proffq = Tracer.getProfiler("preloadPlanCache");

    assignmentsPlanCache = new HashTable();

    String hql = "select wlp.assig.id,year(wlp.inserted)*10000+month(wlp.inserted)*100+day(wlp.inserted), wlp.duration, wlp.action from " + WorklogPlan.class.getName() + " as wlp where " +
      "wlp.inserted>=:pst and wlp.inserted<=:pen";

    if (JSP.ex(resources))
      hql += " and wlp.assig.resource in (:ress)";

    hql += " order by wlp.assig.id,wlp.inserted";

    OqlQuery oql = new OqlQuery(hql);
    if (JSP.ex(resources))
      oql.getQuery().setParameterList("ress", resources);
    oql.getQuery().setTimestamp("pst", period.getStartDate());
    oql.getQuery().setTimestamp("pen", period.getEndDate());
    List<Object[]> o = oql.list();

    for (Object[] oo : o) {
      Map<Integer, Assignment.PlannedWork> assPlan = assignmentsPlanCache.get(oo[0] + "");
      if (assPlan == null) {
        assPlan = new HashTable();
        assignmentsPlanCache.put(oo[0] + "", assPlan);
      }
      int day = ((Number) oo[1]).intValue();
      Assignment.PlannedWork ap = new Assignment.PlannedWork();
      ap.loadByPlan  = ((Number) oo[2]).longValue();
      ap.hasNotes = JSP.ex((String) oo[3]);
      assPlan.put(day, ap);

    }
    proffq.stop();
  }


  /**
   * p1\p2    pe |  e | p |  x
   * pe      D  | -1 | 1 | -1
   * e      1  |  D | 1 | -1
   * p     -1  | -1 | 0 | -1
   * x      1  |  1 | 1 |  0
   */
  private static class AssigComparatorByDensity implements Comparator<Assignment> {
    public int compare(Assignment ass1, Assignment ass2) {
      int ret = 0;

      // from density
      if (ass1.getTask().getDuration() != 0 && ass2.getTask().getDuration() != 0) {
        //here the estimation from issues is ignored

        double d1 = ass1.getEstimatedWorklog() / ass1.getTask().getDuration();
        double d2 = ass2.getEstimatedWorklog() / ass2.getTask().getDuration();
        ret = (int) (d2 - d1);

      } else {
        ret = 0;
      }

      return ret;
    }
  }

  private static class AssigComparatorByDuration implements Comparator<Assignment> {
    public int compare(Assignment ass1, Assignment ass2) {
      int ret = 0;


/*
      // bicch: 14/6/2016 questo dà una Comparison method violates its general contract!
      if (ass1.getTask().getDuration() != 0 && ass2.getTask().getDuration() != 0) {
        //here the estimation from issues is ignored
        ret = ass1.getTask().getDuration() - ass2.getTask().getDuration();

      } else {
        ret = 0;
      }
*/
      ret = ass1.getTask().getDuration() - ass2.getTask().getDuration();

      return ret;
    }
  }


  public static class ResourceLoad {
    public Resource resource;
    public List<Assignment> assignmentsSorted;                              // sorted by weight
    public TreeMap<Integer, Long> resourceWorkPower;                        // millis workable for each day (decreased by unvailability)
    public TreeMap<Integer, LoadAndWork> totalLoadAndWork;           // for each day total planned by plan, by issue and work done   //long[0]= estimation long[1]= work done
    public Map<Assignment, Map<Integer, AssigLoadAndWork>> assignementsDetailLine;    // for each assignment a map for each day with        //long[0]= estimation long[1]= work done long[2]= plan long[3]= 1->hasNote 0->noNote
    public TreeMap<Integer, Set<Task>> milestones;                           // for some day a set of task with milestone in that day

    // statistics for period keys are
    // "maxCapacity" "minCapacity" "meanCapacity"  working day capacity in milliseconds
    // "minDone" "maxDone" "meanDone" worked millis per working day
    // "maxLoad" "minLoad" operator load in millis
    // "meanLoadPerc" operator load percentage
    private Map<String, Double> stats;

    public ResourceLoad(Resource resource) {
      this.resource = resource;
      this.assignementsDetailLine = new HashMap();
      this.milestones = new TreeMap();
    }

    public Map<String, Double> getStats(Period period) {
      if (stats == null)
        stats = new HashTable();

      Double sumCapacity = 0.0;
      Double minCapacity = 0.0;
      Double maxCapacity = 0.0;
      int workingDays = 0;

      CompanyCalendar cc = new CompanyCalendar();
      cc.setTime(period.getStartDate());
      while (cc.getTimeInMillis() <= period.getValidityEndDate().getTime()) {
        int day = DateUtilities.dateToInt(cc.getTime());
        Long wp = resourceWorkPower.get(day);
        workingDays += wp < 0 ? 0 : 1;
        sumCapacity += wp > 0 ? wp : 0;
        minCapacity = Math.min(minCapacity, wp < 0 ? minCapacity : wp);
        maxCapacity = Math.max(maxCapacity, wp < 0 ? maxCapacity : wp);
        cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
      }

      Double sumLoad = 0.0;
      Double minLoad = 0.0;
      Double maxLoad = 0.0;

      Double sumDone = 0.0;
      Double minDone = 0.0;
      Double maxDone = 0.0;

      cc.setTime(period.getStartDate());
      while (cc.getTimeInMillis() <= period.getValidityEndDate().getTime()) {
        int day = DateUtilities.dateToInt(cc.getTime());
        LoadAndWork tlaw = totalLoadAndWork.get(day); //long[0]= estimation long[1]= work done
        //Long wp = resourceWorkPower.get(day); //quanto puoi lavorare quel giorno
        if (tlaw != null) {
          sumLoad += tlaw.workLoad;
          //if (wp!=0) {
          minLoad = Math.min(minLoad, tlaw.workLoad);
          maxLoad = Math.max(maxLoad, tlaw.workLoad);
          //}

          sumDone += tlaw.workDone;
          minDone = Math.min(minDone, tlaw.workDone);
          maxDone = Math.max(maxDone, tlaw.workDone);
        } else {
          Tracer.platformLogger.warn("Workload lenght differes on day:" + day);
        }
        cc.add(CompanyCalendar.DAY_OF_MONTH, 1);
      }

      workingDays = workingDays <= 0 ? 1 : workingDays;

      stats.put("minCapacity", minCapacity);
      stats.put("maxCapacity", maxCapacity);
      stats.put("meanCapacity", sumCapacity / (double) workingDays);

      stats.put("minLoad", minLoad);
      stats.put("maxLoad", maxLoad);
      stats.put("meanLoad", sumLoad / workingDays);
      stats.put("meanLoadPerc", sumCapacity < 1 ? 999.99 : 100.0 * sumLoad / sumCapacity);

      stats.put("minDone", minDone);
      stats.put("maxDone", maxDone);
      stats.put("meanDone", sumDone / (double) workingDays);

      return stats;
    }
  }

  //long[0]= estimation
  // long[1]= work done
  public static class LoadAndWork {
    public long workLoad=0;
    public long workDone=0;
  }

/*
  * long[0]= estimation,  load
  * long[1]= work done
  * long[2]= plan   -1=there is no plan for a day,  0= do not work for that day  x= you must work x hour for that day
  * long[3]= 1-> plan has note 0-> no note
*/
public static class AssigLoadAndWork {
    public long workLoad=0;
    public long loadByPlan=-1; // -1=there is no plan for a day,  0= do not work for that day  x= you must work x hour for that day
    public long loadByIssue=0;
    public long workDone=0;
    public boolean hasNotes=false;

    // considera che il piano è settato a -1 quando non c'è pianificazione
    public long getTotalPlanned(){
      return (loadByPlan<0?0:loadByPlan)+loadByIssue;
    }

}
}
