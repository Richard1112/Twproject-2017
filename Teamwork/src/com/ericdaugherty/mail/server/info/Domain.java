/******************************************************************************
 * This program is a 100% Java Email Server.
 ******************************************************************************
 * Copyright (c) 2001-2013, Eric Daugherty (http://www.ericdaugherty.com)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the copyright holder nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ******************************************************************************
 * For current versions and more information, please visit:
 * http://javaemailserver.sf.net/
 *
 * or contact the author at:
 * andreaskyrmegalos@hotmail.com
 *
 ******************************************************************************
 * This program is based on the CSRMail project written by Calvin Smith.
 * http://crsemail.sourceforge.net/
 ******************************************************************************
 *
 * $Rev: 310 $
 * $Date: 2013-05-13 00:37:09 +0200 (Mo, 13 Mai 2013) $
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.info;

//Java imports
import java.io.Serializable;
import java.util.Locale;

/**
 *
 * @author Andreas Kyrmegalos
 */
public class Domain implements Persistable, Serializable{

   private static final Locale englishLocale = Locale.ENGLISH;
   
   private final static Domain nullDomain = new Domain();
   
   public static Domain getNullDomain() {
      return nullDomain;
   }

   private String domainName = "";
   
   private String uniqueDomainName = "";
   
   private int hashCode = 0;
   
   /**
    * The null domain as used in JES.
    * @see #Domain(String)
    */
   private Domain() {}

   /**
    * A domain as represented in JES. While letter casing
    * is respected and preserved, a domain entry is not case-
    * sensitive. That is, two domain instances whose {@link String}
    * representations are considered equal by effectively an
    * equalsIgnoreCase equality comparison are treated as one
    * and the same.
    * 
    * @param domainName the non-normalized domain name
    */
   public Domain(String domainName) {
      this.domainName = domainName;
      
      uniqueDomainName = domainName.toLowerCase(englishLocale);
      
      hashCode = uniqueDomainName.hashCode();
      if (hashCode == 0) {
         throw new IllegalArgumentException("Use the nullDomain.");
      }
   }

   /**
    * 
    * @return the domainName whose case has been preserved
    */
   public String getDomainName() {
      return domainName;
   }
   
   public String getUniqueName() {
      return uniqueDomainName;
   }

   /**
    * 
    * @return the hashCode
    */
   @Override
   public final int hashCode() {
      return hashCode;
   }

   /**
    * Two polymorphic Domain Objects are treated as representing
    * the same Domain if the case-insensitive {@link String}
    * depiction of the Domain is equal for both objects.
    * 
    * @param object the object to consider whether it is meaningfully equivalent to this instance
    * @return true if this instance of Domain is equivalent to the supplied object parameter
    */
   @Override
   public final boolean equals(Object object) {

      if (object==this) return true;
      if (object==null) return false;
      if (!(object instanceof Domain)) return false;
      Domain that = (Domain)object;
      if ((this.uniqueDomainName.length()==0&&that.uniqueDomainName.length()!=0)||
            (!this.uniqueDomainName.equals(that.uniqueDomainName))) return false;
      return true;
   }
   
   @Override
   public String toString() {
       return "[Domain: "+domainName+"]";
   }
}
