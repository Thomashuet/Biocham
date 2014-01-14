package fr.inria.contraintes.biocham;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;



/**
 * Class that listens to the workbench area events.
 * 
 * @author Dragana Jovanovska  
 */ 
public class WorkbenchActionListener extends AbstractAction{

	
	
	
		private static final long serialVersionUID = 1L;
		public static JTabbedPane tabbedPane;
	   
		public WorkbenchActionListener(String name,String text, Icon icon, String description,char accelerator,Object MnemonicKey) {		
			
			super(name, icon);  
			
	        putValue(SHORT_DESCRIPTION, description);        
	        
	        
	        if(MnemonicKey!=null){	
	        	
	        	putValue(MNEMONIC_KEY,MnemonicKey);
	        	putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(accelerator,Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
	        }
		}
		
		public WorkbenchActionListener(String name) {		
			
			super(name);
			if(name.equals("save")){
				//menuItem=createMenuItem("New Model",KeyEvent.VK_N,bwIcon);
	        	//menuItem.setMnemonic(key);		
	    		//menuItem.setAccelerator(KeyStroke.getKeyStroke(key,Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
				putValue(MNEMONIC_KEY,KeyEvent.VK_S);
	        	//putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S));
			}
		}
	
		
		
		
		
		
		public void actionPerformed(ActionEvent e) {
				
			
			if(getValue(NAME).equals("documentation")){
				
				BiochamMainFrame.showDocumentationScreen();
				
				
	    	}	    	
	    	else if(getValue(NAME).equals("quit")){
	    		
	    		BiochamMainFrame.quit();
    			
	    	}else if(getValue(NAME).equals("save")){
	    		BiochamDynamicTree.currentModel.saveToFile(false);
	    	}
	    	
	    	
	    	
	    }

		   
}
