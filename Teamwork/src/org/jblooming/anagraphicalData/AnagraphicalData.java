package org.jblooming.anagraphicalData;

import com.opnlb.fulltext.Indexable;
import net.sf.json.JSONObject;
import org.apache.lucene.analysis.core.StopAnalyzer;
import org.hibernate.search.annotations.*;
import org.hibernate.search.bridge.builtin.IntegerBridge;
import org.jblooming.logging.Auditable;
import org.jblooming.ontology.HideableIdentifiableSupport;
import org.jblooming.persistence.PersistenceHome;
import org.jblooming.persistence.exceptions.FindByPrimaryKeyException;
import org.jblooming.utilities.JSP;

import java.io.Serializable;


/**
 * <code>AnagraphicalData</code> holds a record for a location reference
 * <br>
 * <p/>
 * <code>locationDescription</code> e.g.: office, home, etc.<br>
 *
 * @author Pietro Polsinelli, Roberto Bicchierai
 * @version 2 alpha
 * @since JDK 1.4
 */

@Indexed(index = "fulltext")
public class AnagraphicalData extends HideableIdentifiableSupport implements Auditable, Indexable {

  private String locationDescription;

  private String address;
  private String zip;
  private String telephone;
  private String mobile;
  private String fax;
  private String email;
  private String url;
  private String googleMapsUrl;
  private String city;
  private String province;
  private String state;
  private String country;
  private int orderFactor = 0;

  private String building;
  private String floor;
  private String room;

  private String otherTelephone;
  private String otherTelDescription;

  private boolean hideAnagraphicalData = false;

  public static final String ANAGRAPHICALDATA = "ANGD";

  public AnagraphicalData() {
  }

  @DocumentId
  @FieldBridge(impl = IntegerBridge.class)
  public Serializable getId() {
    return super.getId();
  }

  /**
   * Method setLocationDescription
   *
   * @param locationDescription a  String
   */
  public void setLocationDescription(String locationDescription) {
    this.locationDescription = locationDescription;
  }

  /**
   * Method getLocationDescription
   *
   * @return a String
   */
  public String getLocationDescription() {
    return locationDescription;
  }

  /**
   * Method setAddress
   *
   * @param address a  String
   */
  public void setAddress(String address) {
    this.address = address;
  }

  /**
   * Method getAddress
   *
   * @return a String
   */
  public String getAddress() {
    return address;
  }

  /**
   * Method setTelephone
   *
   * @param telephone a  String
   */
  public void setTelephone(String telephone) {
    this.telephone = telephone;
  }

  /**
   * Method getTelephone
   *
   * @return a String
   */
  public String getTelephone() {
    return telephone;
  }

  /**
   * Method setFax
   *
   * @param fax a  String
   */
  public void setFax(String fax) {
    this.fax = fax;
  }

  /**
   * Method getFax
   *
   * @return a String
   */
  public String getFax() {
    return fax;
  }

  /**
   * Method setEmail
   *
   * @param email a  String
   */
  public void setEmail(String email) {
    this.email = email;
  }

  /**
   * Method getEmail
   *
   * @return a String
   */
  public String getEmail() {
    return email;
  }

  /**
   * Method setUrl
   *
   * @param url a  String
   */
  public void setUrl(String url) {
    this.url = url;
  }

  /**
   * Method getUrl
   *
   * @return a String
   */
  public String getUrl() {
    return url;
  }


  /**
   * Method setCity
   *
   * @param city a  String
   */
  public void setCity(String city) {
    this.city = city;
  }

  /**
   * Method getCity
   *
   * @return a String
   */
  public String getCity() {
    return city;
  }

  /**
   * Method setState
   *
   * @param state a  String
   */
  public void setState(String state) {
    this.state = state;
  }

  /**
   * Method getState
   *
   * @return a String
   */
  public String getState() {
    return state;
  }

  /**
   * Method setCountry
   *
   * @param country a  String
   */
  public void setCountry(String country) {
    this.country = country;
  }

  /**
   * Method getCountry
   *
   * @return a String
   */
  public String getCountry() {
    return country;
  }

  public String getMobile() {
    return mobile;
  }

  public void setMobile(String mobile) {
    this.mobile = mobile;
  }

  public String getZip() {
    return zip;
  }

  public void setZip(String zip) {
    this.zip = zip;
  }

  public int getOrderFactor() {
    return orderFactor;
  }

  public void setOrderFactor(int orderFactor) {
    this.orderFactor = orderFactor;
  }

  public String getProvince() {
    return province;
  }

  public void setProvince(String province) {
    this.province = province;
  }

  public String getName() {
    return getId() + "";
  }


  public String getBuilding() {
    return building;
  }

  public void setBuilding(String building) {
    this.building = building;
  }

  public String getFloor() {
    return floor;
  }

  public void setFloor(String floor) {
    this.floor = floor;
  }

  public String getRoom() {
    return room;
  }

  public void setRoom(String room) {
    this.room = room;
  }


  public boolean isHideAnagraphicalData() {
    return hideAnagraphicalData;
  }

  public void setHideAnagraphicalData(boolean hideAnagraphicalData) {
    this.hideAnagraphicalData = hideAnagraphicalData;
  }

  /* public static Comparator dataOrder = new Comparator() {
 public int compare(Object o1, Object o2) {
   AnagraphicalData data1 = (AnagraphicalData) o1;
   AnagraphicalData data2 = (AnagraphicalData) o2;
   final int order1 = data1.getOrderFactor();
   final int order2 = data2.getOrderFactor();
   if (order1 == order2) {
     return 0;
   }
   return order1 - order2;
 }
};  */

  public String getOtherTelephone() {
    return otherTelephone;
  }

  public void setOtherTelephone(String otherTelephone) {
    this.otherTelephone = otherTelephone;
  }

  public String getOtherTelDescription() {
    return otherTelDescription;
  }

  public void setOtherTelDescription(String otherTelDescription) {
    this.otherTelDescription = otherTelDescription;
  }

  public String getGoogleMapsUrl() {
    return googleMapsUrl;
  }

  public void setGoogleMapsUrl(String googleMapsUrl) {
    this.googleMapsUrl = googleMapsUrl;
  }

  public String getAbstractForIndexing() {

    return
        JSP.w(getLocationDescription()) + "\n" +
            JSP.w(getAddress()) + " " +
            JSP.w(getZip()) + " " +
            JSP.w(getCity()) + "\n" +
            JSP.w(getEmail()) + "\n" +
            JSP.w(getTelephone()) + " " +
            JSP.w(getFax()) + " " +
            JSP.w(getMobile()) + "\n" +
            JSP.w(getOtherTelDescription()) + " " +
            JSP.w(getOtherTelephone()) + "\n" +
            JSP.w(getUrl());
  }

  @Fields({
          @Field(name = "fullcontent", analyzer = @Analyzer(impl = StopAnalyzer.class)),
          @Field(name = "content")
      })
  private String getContentForIndexing() {
    return getAbstractForIndexing();
  }

  public JSONObject jsonify() {
    JSONObject ret= super.jsonify();

    ret.element("id",getId());
    ret.element("order",getOrderFactor());
    ret.element("location",JSP.w(getLocationDescription()));
    ret.element("address",JSP.w(getAddress()));
    ret.element("city",JSP.w(getCity()));
    ret.element("state",JSP.w(getState()));
    ret.element("province",JSP.w(getProvince()));
    ret.element("country",JSP.w(getCountry()));
    ret.element("zip",JSP.w(getZip()));
    ret.element("url",JSP.w(getUrl()));
    ret.element("email",JSP.w(getEmail()));
    ret.element("mobile",JSP.w(getMobile()));
    ret.element("fax",JSP.w(getFax()));
    ret.element("telephone",JSP.w(getTelephone()));
    
    return ret;
  }

  public static AnagraphicalData load(String id) throws FindByPrimaryKeyException {
    return (AnagraphicalData) PersistenceHome.findByPrimaryKey(AnagraphicalData.class,id);
  }
}
