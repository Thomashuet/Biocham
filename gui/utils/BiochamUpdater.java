/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 * Copyright 2003-2008, INRIA, Projet Contraintes
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * BiochamUpdater.java
 * by Sylvain Soliman */

package fr.inria.contraintes.biocham.utils;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;


import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

/**
 * A class representing an updater for BIOCHAM software. Executes in a separate thread checking the 
 * current version of BIOCHAM and if there is a newer version of it available on the BIOCHAM main web page,
 * it asks the user if he/she accepts to download the newer version. If the user confirms the download, 
 * this class just forwards the user to the download page of BIOCHAM. The download of the newer version
 * is left to the end user's responsibility. In the next version of the BIOCHAM GUI, it will be done by
 * the BIOCHAM environment.
 *           
 * @author Sylvain Soliman         
*/
public class BiochamUpdater extends Thread {
   int state = 0;
   String uri;
   JFrame bc;

   public BiochamUpdater(final String uri, final JFrame bc) {
	
      state = 0;
      this.uri=uri;
      this.bc=bc;
      
      SwingWorker sw=new SwingWorker(){

		@Override
		public Object construct() {
			 try {
			    	
 	    		
		         SAXParser saxParser = SAXParserFactory.newInstance().newSAXParser();
		         saxParser.parse(uri, (DefaultHandler) new DefaultHandler() {
		        	 
		        	 
		        	 
		            public void startElement(String uri, String localName, String qName, Attributes attrs) {
		            	
		            	for (int i=0; i < attrs.getLength(); i++) {
//			        		 recuperation du nom de l'attribut et de sa valeur
			        		String attName = attrs.getQName(i);
			        		if ("".equals(attName)) attName = attrs.getQName(i);
			        		Utils.debugMsg("\tattribut["+i+"]: "+ attName + "=" +attrs.getValue(i)+"\n");
		            	}		            	
		            	if (qName.equals("item")) {
		            		state++;
		            	} else if (qName.equals("title")) {
		            		state++;
		            	}
		            }
		            
		            public void endElement(String uri, String localName, String qName) {
		               if (state==3) {
		                  state++;
		               }
		            }
		            public void characters(char[] buf, int offset, int len) {
		               if (state==3) {
		                  String version=new String(buf, offset, len).substring(8);
		               
		                  if (Float.parseFloat(version) > Float.parseFloat(Utils.BIOCHAM_VERSION) &&
		                     JOptionPane.showConfirmDialog(bc,
		                        "Version "+version+" of BIOCHAM seems to be available, do you want to download it ?",
		                     "Confirmation",
		                     JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
		                     BrowserLauncher.openURL("http://contraintes.inria.fr/BIOCHAM/"); //workbench.browseUrl("http://contraintes.inria.fr/BIOCHAM/"); 
		                  }
		               }
		            }
		         
		         });
		      } catch (Throwable t) {
		      }
			return null;
		}

		@Override
		public void finished() {
			Utils.debugMsg("FINISHED!!!!!!!!!!!!!!!!");
			
		}};
		
		sw.start();
   }

   public void run() {
     
	   try {
    	
    	    		
         SAXParser saxParser = SAXParserFactory.newInstance().newSAXParser();
         saxParser.parse(uri, (DefaultHandler) new DefaultHandler() {
        	 
            public void startElement(String uri, String localName, String qName, Attributes attrs) {
               if (qName.equals("item")) {
                  state++;
               } else if (qName.equals("title")) {
                  state++;
               }
            }
            public void endElement(String uri, String localName, String qName) {
               if (state==3) {
                  state++;
               }
            }
            public void characters(char[] buf, int offset, int len) {
               if (state==3) {
                  String version=new String(buf, offset, len).substring(8);
               
                  if (Float.parseFloat(version) > Float.parseFloat(Utils.BIOCHAM_VERSION) &&
                     JOptionPane.showConfirmDialog(bc,
                        "Version "+version+" of BIOCHAM seems to be available, do you want to download it ?",
                     "Confirmation",
                     JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
                     BrowserLauncher.openURL("http://contraintes.inria.fr/BIOCHAM/"); //workbench.browseUrl("http://contraintes.inria.fr/BIOCHAM/"); 
                  }
               }
            }
         
         });
      } catch (Throwable t) {
      }
   }
   
}
