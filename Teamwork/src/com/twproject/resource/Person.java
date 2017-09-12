package com.twproject.resource;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.annotations.Type;
import org.hibernate.search.annotations.Analyzer;
import org.hibernate.search.annotations.Boost;
import org.hibernate.search.annotations.DocumentId;
import org.hibernate.search.annotations.Field;
import org.hibernate.search.annotations.FieldBridge;
import org.hibernate.search.annotations.Fields;
import org.hibernate.search.annotations.Indexed;
import org.hibernate.search.bridge.builtin.StringBridge;
import org.jblooming.logging.Auditable;
import org.jblooming.ontology.PlatformComparators;
import org.jblooming.oql.OqlQuery;
import org.jblooming.oql.QueryHelper;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.exceptions.StoreException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.utilities.JSP;
import org.jblooming.waf.view.RestState;

import com.opnlb.fulltext.Indexable;
import com.twproject.messaging.stickyNote.StickyNote;
import com.twproject.operator.TeamworkOperator;

import net.sf.json.JSONObject;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
@Indexed(index = "fulltext")
@Boost(1.5f)
public class Person extends Resource implements Indexable, Auditable {

  private String personName;
  private String personSurname;
  private String courtesyTitle;
  private Date hiringDate;
  private String personalInterest;
  private TeamworkOperator myself;
  private String blackBoardNotes;

  private List news = new ArrayList();
  // read only colls
  private Set workerEvents;
  private Set stickyAuthored = new HashSet();
  private Set stickyReceived = new HashSet();
  private Set eventOwned = new HashSet();
  private Set eventAuthored = new HashSet();


	@Override
	@Type(type = "string")
  @DocumentId
  @FieldBridge(impl = StringBridge.class)
  public Serializable getId() {
    return super.getId();
  }

  public String getPersonName() {
    return personName;
  }
  public  void setPersonName(String personName) {
    this.personName = personName;
  }


  public String getPersonSurname() {
    return personSurname;
  }

  public void setPersonSurname(String personSurname) {
    this.personSurname = personSurname;
  }


  public String getCourtesyTitle() {
    return courtesyTitle;
  }

  public void setCourtesyTitle(String courtesyTitle) {
    this.courtesyTitle = courtesyTitle;
  }

  public Date getHiringDate() {
    return hiringDate;
  }

  public void setHiringDate(Date hiringDate) {
    this.hiringDate = hiringDate;
  }

  public String getPersonalInterest() {
    return personalInterest;
  }

  public void setPersonalInterest(String personalInterest) {
    this.personalInterest = personalInterest;
  }


	@Override
	public Set<Person> getPersons(Set<Resource> visitedNodes, Set<Person> workers) {
    Set i = new HashSet();
    if (!isHidden())
      i.add(this);
    return i;
  }


	@Override
	public boolean isPersonIn(Person o) {
    return this.equals(o);
  }

  private Set getStickyAuthored() {
    return stickyAuthored;
  }

  private void setStickyAuthored(Set stickyAuthored) {
    this.stickyAuthored = stickyAuthored;
  }

  public Iterator getStickyAuthoredIterator() {
    return stickyAuthored.iterator();
  }

  public boolean stickyAuthoredContains(StickyNote authored) {
    return stickyAuthored.contains(authored);
  }

  public int stickyAuthoredSize() {
    return stickyAuthored.size();
  }

  private Set getStickyReceived() {
    return stickyReceived;
  }

  private void setStickyReceived(Set stickyReceived) {
    this.stickyReceived = stickyReceived;
  }

  public Iterator getStickyReceivedIterator() {
    return stickyReceived.iterator();
  }

  public boolean stickyReceivedContains(StickyNote received) {
    return stickyReceived.contains(received);
  }

  public int stickyReceivedSize() {
    return stickyReceived.size();
  }

  private Set getEventOwned() {
    return eventOwned;
  }

  private void setEventOwned(Set eventOwned) {
    this.eventOwned = eventOwned;
  }

  public Iterator getEventOwnersIterator() {
    return eventOwned.iterator();
  }

  public boolean eventOwnersContains(Resource eventOwner) {
    return eventOwned.contains(eventOwner);
  }

  public int eventOwnersSize() {
    return eventOwned.size();
  }

  private Set getEventAuthored() {
    return eventAuthored;
  }

  private void setEventAuthored(Set eventAuthored) {
    this.eventAuthored = eventAuthored;
  }

  public Iterator getEventAuthorsIterator() {
    return eventAuthored.iterator();
  }

  public boolean eventAuthorsContains(Resource eventAuthor) {
    return eventAuthored.contains(eventAuthor);
  }

  public int eventAuthorsSize() {
    return eventAuthored.size();
  }

  private List getNews() {
    return news;
  }

  private void setNews(List news) {
    this.news = news;
  }

  public void addNews(String notice) {
    news.add(notice);
  }

  public void removeNews(String notice) {
    news.remove(notice);
  }

  public Iterator getNewsIterator() {
    return news.iterator();
  }

  private Set getWorkerEvents() {
    return workerEvents;
  }

  private void setWorkerEvents(Set workerEvents) {
    this.workerEvents = workerEvents;
  }

  public int eventsSize() {
    return workerEvents.size();
  }

	@Override
	public TeamworkOperator getMyself() {
    return myself;
  }

  public void setMyself(TeamworkOperator myself) {
    this.myself = myself;
  }

  public String getBlackBoardNotes() {
    return blackBoardNotes;
  }

  public void setBlackBoardNotes(String blackBoardNotes) {
    this.blackBoardNotes = blackBoardNotes;
  }


	@Override
	public String getDisplayName() {
		return (JSP.ex(getCourtesyTitle()) ? JSP.w(getCourtesyTitle()) + " " : "") + JSP.w(getPersonSurname()) + " "
				+ JSP.w(getPersonName());
  }


  public TreeSet<Resource> getAllMyStaff() throws PersistenceException {
		TreeSet<Resource> totalStaff = new TreeSet<>(new PlatformComparators.NameComparator());

    //get my staff
    //use manager ids for performance
    String hql = "select resource from " + Resource.class.getName() + " as resource";
    QueryHelper queryHelper= new QueryHelper(hql);


    ResourceBricks.addMyStaffQueryClause("resource", queryHelper, this);

    queryHelper.addToHqlString("order by resource.name");


    List<Resource> staff = queryHelper.toHql().list();
    totalStaff.addAll(staff);

    return totalStaff;
  }

  public static Person getLoggedPerson(RestState pageState) throws PersistenceException {
    return ((TeamworkOperator)pageState.getLoggedOperator()).getPerson();
  }

  public static Person getPersonFromOperatorId(String opId) throws PersistenceException {
    String hql = "select person from " + Person.class.getName() + " as person where person.myself.id=:opid";
    OqlQuery oql = new OqlQuery(hql);
    oql.getQuery().setString("opid", opId);
    return (Person) oql.uniqueResult();
  }

	@Override
	public String getAbstractForIndexing() {
    String resourceAbstract=super.getAbstractForIndexing();
    return resourceAbstract;
  }

  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
  })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }


  public static Person load(Serializable id) throws FindByPrimaryKeyException {
    return (Person) PersistenceHome.findByPrimaryKey(Person.class, id);
  }


	@Override
	public void store(PersistenceContext pc) throws StoreException {
		setName(JSP.w(getPersonName()) + (JSP.ex(getPersonSurname()) ? (JSP.w(getPersonSurname()) + " ") : ""));
    super.store(pc);
    if (getMyself()!=null){
      getMyself().setName(getPersonName());
      getMyself().setSurname(getPersonSurname());

    }
  }


	@Override
	public JSONObject jsonify() {
    return jsonify(false);
  }

	@Override
	public JSONObject jsonify(boolean fullLoading) {
    JSONObject ret = super.jsonify(fullLoading);
    ret.element("name", getPersonName());
    ret.element("surname",getPersonSurname());

    return ret;
  }


}