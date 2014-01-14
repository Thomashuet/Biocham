package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.graphicalEditor.GraphElementsComboBox;
import net.java.balloontip.BalloonTip;

import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicComboPopup;
import javax.swing.plaf.basic.ComboPopup;
import javax.swing.plaf.metal.MetalComboBoxUI;
 




/**
 * Class thats creates a custom JComboBox with the possibility of multi selection.
 * 
 * @author Dragana Jovanovska  
 */ 
public class CustomMultiSelectComboBox extends JComboBox{
	
	 
     JComboBox myComboBox;
     protected int popupWidth;
     CustomComboBox parentClass;
     BalloonTip bTip;
     SteppedComboBoxUI scbxUI;
     
     public CustomMultiSelectComboBox(CustomComboBox parent)
     {
    	 
    		boolean b=SwingUtilities.isEventDispatchThread();
    		
    		
         myComboBox = new JComboBox();
         scbxUI=new SteppedComboBoxUI();
         
         setUI(scbxUI);
         popupWidth = 0;
         parentClass=parent;
         
         
     }
     
    
     
     public CustomMultiSelectComboBox(GraphElementsComboBox parent)
     {
    	 
    		boolean b=SwingUtilities.isEventDispatchThread();
    		
    		
         myComboBox = new JComboBox();
         scbxUI=new SteppedComboBoxUI();
         
         setUI(scbxUI);
         popupWidth = 0;
         //parentClass=parent;
         
         
     }
     
     
     public void removebTip(){
    	 bTip.setEnabled(false);
    	 bTip.setVisible(false);
     }
     
     public void hidePopup(){
    	 scbxUI.popup.setVisible(false);
     }
     
     public void setPopupVisible(boolean b)
     {
    	
    	 if(b)
    	 {
    		 myComboBox.showPopup();
    	 }
    	 //
     }
     public void setPopupWidth(int width) {
 	    popupWidth = width;
     }

     public Dimension getPopupSize() {
 		 Dimension size = getSize();
 		 if (popupWidth < 1) popupWidth = size.width;
 		 return new Dimension(popupWidth, size.height);
 	 }
   
  
     public CustomComboBox getParentClass() {
    	 return parentClass;
		}
     public void setParentClass(CustomComboBox parentClass) {
    	 this.parentClass = parentClass;
     }
	
	

	public BalloonTip getBTip() {
		return bTip;
	}

	public void setBTip(BalloonTip tip) {
		bTip = tip;
	}
	
}

class SteppedComboBoxUI extends MetalComboBoxUI {
	
	public JList l;	
	BasicComboPopup popup;
	
 	  protected ComboPopup createPopup() {
 	    popup = new BasicComboPopup( comboBox ) {

 	    	
 	      public void show() {
 	    	  
 	    	
 	    	l=list;
 	    	clearComboSelection();
 	        Dimension popupSize = ((CustomMultiSelectComboBox)comboBox).getPopupSize();
 	        popupSize.setSize( popupSize.width,getPopupHeightForRowCount( comboBox.getMaximumRowCount() ) );
 	        Rectangle popupBounds = computePopupBounds( 0,comboBox.getBounds().height, popupSize.width, popupSize.height);
 	        scroller.setMaximumSize( popupBounds.getSize() );
 	        scroller.setPreferredSize( popupBounds.getSize() );
 	        scroller.setMinimumSize( popupBounds.getSize() );
 	        list.invalidate();
 	       /* int selectedIndex = comboBox.getSelectedIndex();
 	        if ( selectedIndex == -1 ) {
 	          list.clearSelection();
 	        } else {
 	          list.setSelectedIndex( selectedIndex );
 	        }
 	        list.ensureIndexIsVisible( list.getSelectedIndex() );*/
 	        setLightWeightPopupEnabled( comboBox.isLightWeightPopupEnabled() );

 	        show( comboBox, popupBounds.x, popupBounds.y );
 	      }

		/**
		 * 
		 */
		
 	    };
 	    popup.getAccessibleContext().setAccessibleParent(comboBox);
 	    return popup;
 	  }
 	  
 	 public void clearComboSelection() {
 		l.clearSelection();
 	    l.invalidate();
 	}
 	 
}


