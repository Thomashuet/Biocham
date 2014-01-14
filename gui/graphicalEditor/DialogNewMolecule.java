package fr.inria.contraintes.biocham.graphicalEditor;

import net.java.balloontip.BalloonTip;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.customComponents.CustomComboBox;
import fr.inria.contraintes.biocham.customComponents.CustomMultiSelectComboBox;
import fr.inria.contraintes.biocham.customComponents.BackgroundImagePanel;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.customComponents.SimpleComboBox;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

public class DialogNewMolecule extends JDialog{

	
	JLabel initConcL, nameL, stateL, typeL, locationL;
	JButton ok, cancel;
	JTextField initConTF,nameTF, stoichCoef;
	JComboBox stateCB, typeCB, locationCB;	
	String[] types=new String[]{"Macromolecule", "Nucleic Acid Feature", "Complex"};
	JCheckBox multimer;
	NewEntity newEntityData;
	boolean disposed=false;
	BalloonTip validationTip;
	ReactionDialog rDialog;
	ArrayList<String> chosen;
	BiochamGraph graph;
	
	public DialogNewMolecule(final JFrame parent, BiochamGraph g){
		super(parent,true);
		
		graph=g;
		
		
		this.setTitle("Create New Biochemical Entity");
		((java.awt.Frame)this.getOwner()).setIconImage(Icons.images.get("greenIcon.png"));
		
		rDialog=(ReactionDialog)parent;
		
		//parent.setIconImage(Icons.images.get("greenIcon.png"));
		Container contents=this.getContentPane();
		contents.setLayout(new BorderLayout());
		BackgroundImagePanel panel=new BackgroundImagePanel();
		try {
			panel.setBackgroundImage("projectImages/5.jpg");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		panel.setBackgroundPosition(BackgroundImagePanel.STRETCHED);
		
		contents.add(panel,BorderLayout.CENTER);
		
		SpringLayout layout=new SpringLayout();
		panel.setLayout(layout);
		
		typeL=new JLabel("Type: ");
		typeCB=new JComboBox(types);
		typeCB.setSelectedIndex(0);
		typeCB.addItemListener(new ItemListener(){

			public void itemStateChanged(ItemEvent e) {
				String item=e.getItem().toString();		
				if(item.equals("Complex")){
					ChooseContainingMolecules cc=new ChooseContainingMolecules();
					chosen=cc.getChosenMolecules();
					
				}
				
			}});		
		
		typeCB.addMouseListener(new MouseListener(){

			public void mouseClicked(MouseEvent e) {
				if(e.getSource() instanceof JComboBox){
					JComboBox cb=(JComboBox)e.getSource();
					String item=cb.getSelectedItem().toString();
			
				}			
				
				
			}

			public void mouseEntered(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseExited(MouseEvent e) {
				/*if(e.getSource() instanceof JComboBox){
					JComboBox cb=(JComboBox)e.getSource();
					String item=cb.getSelectedItem().toString();
			
				}	*/
				
			}

			public void mousePressed(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseReleased(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

		});
				
		//location
		locationL=new JLabel("Location: ");
		ECompartmentCell[] cells=GraphUtilities.getAllCompartments(graph);
		HashSet<String> locs=new HashSet<String>();
		for(int i=0; i<cells.length;i++){
			if((cells[i] instanceof ECompartmentCell)){
				locs.add(((BiochamCompartmentData)cells[i].getUserObject()).getCompartmentName());
			}
		}
		cells=null;
		String[] locations=new String[locs.size()+2];
		locations[0]="Default";
		Object[] array=locs.toArray();
		int siz=locs.size();
		int i;
		for(i=0;i<siz;i++){
			locations[i+1]=array[i].toString();
		}
		locations[i+1]="Create new";
		locationCB=new JComboBox(locations);
		locationCB.setSelectedIndex(0);
		locationCB.addItemListener(new ItemListener(){

			public void itemStateChanged(ItemEvent e) {
				String sel=e.getItemSelectable().getSelectedObjects()[0].toString();
				
				if(e.getStateChange()==ItemEvent.SELECTED && sel.equals("Create new")){
					String newValue= (String)JOptionPane.showInputDialog(parent,"Enter compartment name:\n",""); 
					if(newValue!=null && newValue!=""){
						locationCB.addItem(newValue);
						locationCB.setSelectedItem(newValue);
					}
				}
				
			}});
		multimer = new JCheckBox("Multimer");
		multimer.setSelected(false);
		multimer.setBackground(Color.WHITE);
		multimer.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				if(e.getSource() instanceof JCheckBox){
					JCheckBox cb=(JCheckBox)e.getSource();
					stoichCoef.setEnabled(cb.isSelected());
				}
				
			}
			
		});
		stoichCoef=new JTextField(" Units>1 ",5);
		stoichCoef.setEnabled(false);
		
		stateL=new JLabel("State: ");
		stateCB=new JComboBox(BiochamGraph.states);
		stateCB.setSelectedIndex(1);
		stateCB.addItemListener(new ItemListener(){

			public void itemStateChanged(ItemEvent e) {
				String sel=e.getItemSelectable().getSelectedObjects()[0].toString();
				
				if(e.getStateChange()==ItemEvent.SELECTED && sel.equals("Modified")){
					String newValue= (String)JOptionPane.showInputDialog(parent,"Enter the modification sites:\n",""); 
					if(newValue!=null && newValue!=""){
						stateCB.setName(newValue);
					}else{
						stateCB.setName(null);
					}
				}
				
			}});
		stateCB.setBounds(stateCB.getX(),stateCB.getY(),typeCB.getWidth(), stateCB.getHeight());
		nameL=new JLabel("Name: ");
		nameTF=new JTextField(" Enter Name ",14);
		final BalloonTip bTip=new BalloonTip(nameTF,"The molecule name cannot contain empty spaces!",Utils.molecule,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
		bTip.setText("The molecule name cannot contain empty spaces!");
		bTip.setIcon(Icons.icons.get("warning_16.png"));
		bTip.setIconTextGap(10);	
		bTip.setVisible(false);
		bTip.enableClickToHide(true);
		validationTip=bTip;
		nameTF.addMouseListener(new MouseListener(){

			public void mouseClicked(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseEntered(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseExited(MouseEvent e) {
				if(e.getSource() instanceof JTextField){
					String t=nameTF.getText();
					if(t.contains(" ")){
						ok.setEnabled(false);
						validationTip.setVisible(true);	
						nameTF.setText(t.trim());
					}else{
						ok.setEnabled(true);
					}
				}
				
			}

			public void mousePressed(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseReleased(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

		});
		nameTF.addFocusListener(new FocusListener(){

			public void focusGained(FocusEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void focusLost(FocusEvent e) {
				if(e.getSource() instanceof JTextField){
					String t=nameTF.getText();
					if(t.contains(" ")){
						ok.setEnabled(false);
						bTip.setVisible(true);	
					}
				}
				
			}});
	
		initConcL=new JLabel("Initial State: ");
		initConTF=new JTextField(" 0 ",7);
		
		panel.add(typeL);
		panel.add(typeCB);
		
		layout.putConstraint(SpringLayout.WEST, typeL, 95,SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, typeL, 140, SpringLayout.NORTH, panel);
		layout.putConstraint(SpringLayout.WEST, typeCB, 35,SpringLayout.EAST, typeL);
		layout.putConstraint(SpringLayout.NORTH, typeCB, 138, SpringLayout.NORTH, panel);
	
		
		panel.add(multimer);
		panel.add(stoichCoef);
		
		layout.putConstraint(SpringLayout.WEST, multimer, -2,SpringLayout.WEST, typeCB);
		layout.putConstraint(SpringLayout.NORTH, multimer, 13, SpringLayout.SOUTH, typeL);
		layout.putConstraint(SpringLayout.WEST, stoichCoef, 10,SpringLayout.EAST, multimer);
		layout.putConstraint(SpringLayout.NORTH, stoichCoef, 2, SpringLayout.NORTH, multimer);
		
		
		panel.add(stateL);
		panel.add(stateCB);
		
		layout.putConstraint(SpringLayout.WEST, stateL, 0,SpringLayout.WEST, typeL);
		layout.putConstraint(SpringLayout.NORTH, stateL, 10, SpringLayout.SOUTH, multimer);
		layout.putConstraint(SpringLayout.WEST, stateCB, 0,SpringLayout.WEST, typeCB);
		layout.putConstraint(SpringLayout.NORTH, stateCB, -2, SpringLayout.NORTH, stateL);
		
		
		panel.add(locationL);
		panel.add(locationCB);
		
		layout.putConstraint(SpringLayout.WEST, locationL, 0,SpringLayout.WEST, stateL);
		layout.putConstraint(SpringLayout.NORTH, locationL, 15, SpringLayout.SOUTH, stateL);
		layout.putConstraint(SpringLayout.WEST, locationCB, 0,SpringLayout.WEST, stateCB);
		layout.putConstraint(SpringLayout.NORTH, locationCB,  -2, SpringLayout.NORTH, locationL);
		
				
		panel.add(nameL);
		panel.add(nameTF);
		
		layout.putConstraint(SpringLayout.WEST, nameL, 0,SpringLayout.WEST, locationL);
		layout.putConstraint(SpringLayout.NORTH, nameL, 25, SpringLayout.SOUTH, locationL);
		layout.putConstraint(SpringLayout.WEST, nameTF, 0,SpringLayout.WEST, locationCB);
		layout.putConstraint(SpringLayout.NORTH, nameTF, 0, SpringLayout.NORTH, nameL);
		
		
		
		panel.add(initConcL);
		panel.add(initConTF);
		
		layout.putConstraint(SpringLayout.WEST, initConcL, 0,SpringLayout.WEST, nameL);
		layout.putConstraint(SpringLayout.NORTH, initConcL, 15, SpringLayout.SOUTH, nameL);
		layout.putConstraint(SpringLayout.WEST, initConTF,  10,SpringLayout.EAST, initConcL);
		layout.putConstraint(SpringLayout.NORTH, initConTF, 0, SpringLayout.NORTH, initConcL);
			
		
		
		ok=new JButton("OK");
		ok.addMouseListener(new MouseListener(){

			public void mouseClicked(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseEntered(MouseEvent e) {
				if(nameTF.getText().contains(" ")){
					bTip.setVisible(true);
					ok.setEnabled(false);
				}else{
					ok.setEnabled(true);
				}
				
			}

			public void mouseExited(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mousePressed(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}

			public void mouseReleased(MouseEvent e) {
				// TODO Auto-generated method stub
				
			}});
		ok.setActionCommand("OK");
		ok.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});	
		cancel=new JButton("Cancel");
		cancel.setActionCommand("CANCEL");
		cancel.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});		
		panel.add(ok);
		panel.add(cancel);
		layout.putConstraint(SpringLayout.WEST, ok, 150-ok.getWidth(),SpringLayout.WEST, panel);
		layout.putConstraint(SpringLayout.NORTH, ok, 50, SpringLayout.SOUTH, initConTF);
		layout.putConstraint(SpringLayout.WEST, cancel, 10,SpringLayout.EAST, ok);
		layout.putConstraint(SpringLayout.NORTH, cancel, 0, SpringLayout.NORTH, ok);
		
		
		setSize(new Dimension(410, 440));	   
		setResizable(false);
		setLocationRelativeTo(parent);		
		setVisible(true);	
	   
	}

	public NewEntity getNewEntityData() {
		return newEntityData;
	}

	public void setNewEntityData(NewEntity newEntityData) {
		this.newEntityData = newEntityData;
	}
	
	
	 private void windowAction(Object actionCommand) {
	     
		   
		  boolean closeWindow = false;
	      String cmd = null;
	      
	      
	      if (actionCommand != null) {
	    	  
	         if (actionCommand instanceof ActionEvent) {
	        	 
	            cmd = ((ActionEvent)actionCommand).getActionCommand();
	            
	         } else {
	        	 
	            cmd = actionCommand.toString();
	            
	         }
	      }
	      
	      if (cmd == null) {
	         // do nothing
	      } else if (cmd.equals("CANCEL")) {
	    	  
	    	  closeWindow = true;
	    	  
	      } else if (cmd.equals("OK")) {
	    	  
	    	  		
			
						newEntityData=new NewEntity();
						String name=nameTF.getText().trim();
						if(typeCB.getSelectedItem().toString().contains("Nucleic")){
							if(!name.startsWith("#")){							
								newEntityData.setRepresentingName(nameTF.getText().trim());
								name="#"+name;
								newEntityData.setName(name);
							}						
						}
						try{
							if(stoichCoef.isEnabled() && multimer.isSelected() && Integer.valueOf(stoichCoef.getText().trim())>1){							
								newEntityData.setMultimerCoef(Integer.valueOf(stoichCoef.getText().trim()));
							}else{
								newEntityData.setMultimerCoef(0);
							}
						}catch(Exception e){
							newEntityData.setMultimerCoef(0);
						}
						
						if(newEntityData.getMultimerCoef()>1){
							
							String nm=name;
							for(int i=0;i<newEntityData.getMultimerCoef()-1;i++){
								name+="-"+nm;
							}
							nm=null;							
						}	
						if(stateCB.getName()!=null && stateCB.getName()!=""){					
							newEntityData.setModificationSites(stateCB.getName());
							name+="~{"+newEntityData.getModificationSites()+"}";
						}
						
						newEntityData.setName(name);
						if(GraphUtilities.getCellByName(graph,name.trim())!=null){
		    	  			JOptionPane.showMessageDialog(this,"This molecule already exists.");
		    	  		}else{
		    	  			newEntityData.setInitialConcentration(initConTF.getText());
							newEntityData.setState(stateCB.getSelectedItem().toString());
							newEntityData.setType(typeCB.getSelectedItem().toString());
						
							if(typeCB.getSelectedItem().toString().equals("Complex")){
								if(chosen!=null){
									if(chosen.size()>0){
										ArrayList<ContainingMolecule> cMols=new ArrayList<ContainingMolecule>(); 
										for(int i=0;i<chosen.size();i++){
											if(rDialog.graph!=null){
												DefaultGraphCell cell=GraphUtilities.getCellByName(rDialog.graph,chosen.get(i).toString());
												if(cell!=null){									
													BiochamEntityData d=(BiochamEntityData)cell.getUserObject();
													String type="";
													if(cell instanceof ENucleicAcidFeatureCell){
														type="NucleicAcidFeature";
													}else if(cell instanceof EComplexCell){
														type="Complex";
													}else {
														type="Macromolecule";
													}
													String nm1=d.getName();
													if(d.getMultimerCardinality()>1 && nm1.contains("-")){
														String tmpStr=d.getName().substring(d.getName().lastIndexOf("-")+1,d.getName().length());	
														nm1=d.getName().substring(0,d.getName().indexOf(tmpStr))+tmpStr;
													}
													d.setRepresentingName(nm1);
													d.setMoleculeType(type);
													ContainingMolecule cm=new ContainingMolecule(d.getName(),d.getMoleculeState(),d.getMultimerCardinality(),d.isModulator(),type,d.getInitialConcentration(),nm1,d.getId(),cell);
													cMols.add(cm);
												}/*else{
													BiochamEntityData d=(BiochamEntityData)cell.getUserObject();
													d.setName(chosen.get(i).toString());
													if(d.getName().startsWith("#")){
														cell=new ENucleicAcidFeatureCell(d);
														d.setMoleculeType("NucleicAcidFeature");
													}else{
														cell=new EMacromoleculeCell(d);
														d.setMoleculeType("Macromolecule");
													}		
													String nm1=d.getName();
													if(d.getMultimerCardinality()>1 && nm1.contains("-")){
														String tmpStr=d.getName().substring(d.getName().lastIndexOf("-")+1,d.getName().length());	
														nm1=d.getName().substring(0,d.getName().indexOf(tmpStr))+tmpStr;
													}
													d.setRepresentingName(nm1);
													ContainingMolecule cm=new ContainingMolecule(d.getName(),MoleculeState.NONE,0,false,d.getMoleculeType(),d.getInitialConcentration(),nm1,d.getId(),cell);
													cMols.add(cm);
													
												}*/
											}
										}
										newEntityData.setContainingMolecules(cMols);
									}
								}
							}
							Utils.debugMsg("LOCATION SELECTED IS: "+locationCB.getSelectedItem().toString());
							newEntityData.setLocation(locationCB.getSelectedItem().toString());
							if(locationCB.getSelectedItem().toString()!=null && locationCB.getSelectedItem().toString()!="" && !locationCB.getSelectedItem().toString().equals("Default")){
								newEntityData.setName(name+"::"+locationCB.getSelectedItem().toString());
							}
							newEntityData.setRepresentingName(nameTF.getText().trim());
							setNewEntityData(newEntityData);
		    	  		}
						
						
						/*String s="present("+newEntityData.getName()+","+newEntityData.getInitialConcentration()+").\n";
						graph.getBiochamModel().sendToBiocham(s,"initConc");*/
						closeWindow=true;
	    	  		//}
					
	      }
	      
	      
	      
	      
	      
	      if (closeWindow) {
	         setVisible(false);
	         dispose();
	         disposed=true;
	      }
	      
	      
	      
	   }

	public boolean isDisposed() {
		return disposed;
	}

	public void setDisposed(boolean disposed) {
		this.disposed = disposed;
	}
	
	
	public void validateFailed() {
		validationTip.setVisible(true);
		ok.setEnabled(false);
		
		
	}
	
	public void validatePassed() {
		ok.setEnabled(true);
		
	}

	public BalloonTip getValidationTip() {
		return validationTip;
	}

	public void setValidationTip(BalloonTip validationTip) {
		this.validationTip = validationTip;
	}
	
	
	class ChooseContainingMolecules extends JDialog{
	
		
		ArrayList<String> chosenMolecules;
		
		public ChooseContainingMolecules(){
			super(rDialog);
			this.setModal(true);
			super.setModal(true);
			super.setBackground(Utils.backgroundColor);
			this.setLocationRelativeTo(rDialog);
			setTitle("Complex's Containing Molecules");
			setBackground(Utils.backgroundColor);
			JPanel main=new GradientPanel();
			SpringLayout lay=new SpringLayout();
			main.setLayout(lay);
			main.setBackground(Utils.backgroundColor);
			setLayout(new BorderLayout());
			JLabel t=new JLabel("Choose the containing molecules for the Complex:");
			main.add(t);
			String[] list=new String[rDialog.modulatorComboBox.getItemCount()];
			for(int i=0;i<rDialog.modulatorComboBox.getItemCount();i++){
				list[i]=rDialog.modulatorComboBox.getItemAt(i).toString();
			}
			
			CustomMultiSelectComboBox cb=(new CustomComboBox(list,false)).getMultiSelectComboBox();			
			BalloonTip bTip=new BalloonTip(cb,"Don't forget to press ENTER to apply the selection.",Utils.modern,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
			bTip.setText("Don't forget to press ENTER to apply the selection.");
			bTip.setIcon(Icons.icons.get("flag_blue.png"));
			bTip.setIconTextGap(10);	
			bTip.setVisible(false);
			bTip.enableClickToHide(true);	
			((CustomMultiSelectComboBox)cb).setBTip(bTip);			  
			Dimension d = cb.getPreferredSize();
			((CustomMultiSelectComboBox)cb).setPreferredSize(new Dimension(130, d.height));
			((CustomMultiSelectComboBox)cb).setPopupWidth(d.width+20);      
			cb.addPopupMenuListener(new PopupMenuListener(){

				public void popupMenuCanceled(PopupMenuEvent e) {
					if(e.getSource() instanceof CustomMultiSelectComboBox){
						CustomMultiSelectComboBox cb=(CustomMultiSelectComboBox)e.getSource();
						int k=cb.getParentClass().getSelected().size();
						if(k>0){
							cb.getBTip().setVisible(true);
						}else{
							cb.getBTip().setVisible(false);
						}
					}else if(e.getSource() instanceof SimpleComboBox){
						SimpleComboBox cb=(SimpleComboBox)e.getSource();
						int ind=cb.getSelectedIndex();
						if(ind>0){
							cb.getBTip().setVisible(true);
						}else{
							cb.getBTip().setVisible(false);
						}
					}			
				}

				public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {}
				

				public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
					if(e.getSource() instanceof CustomMultiSelectComboBox){
						CustomMultiSelectComboBox cb=(CustomMultiSelectComboBox)e.getSource();
						cb.getBTip().setVisible(false);	
					}else if(e.getSource() instanceof SimpleComboBox){
						SimpleComboBox cb=(SimpleComboBox)e.getSource();
						cb.getBTip().setVisible(true);	
					}			
				}
				});	
			cb.addKeyListener(new KeyAdapter(){

				
				public void keyPressed(KeyEvent e) {

					if(e.getSource() instanceof CustomMultiSelectComboBox){
						
						CustomMultiSelectComboBox cb=(CustomMultiSelectComboBox)e.getSource();				
						if(e.getKeyCode()==KeyEvent.VK_ENTER){	
							
							
							
							cb.hidePopup();							
							cb.getParentClass().setFinished(true);    				
		    				ArrayList selected=cb.getParentClass().getSelected();
		    				ArrayList all = new ArrayList(cb.getParentClass().getAllItems());
		    				boolean allSelected=false;		    				
		    				for(int i=0;i<selected.size();i++){
		    					
		    					
		    					if(selected.contains("Select All")){
		    						
		    						if(cb.getItemCount()<cb.getParentClass().getAllItems().size()){
		    							cb.removeAllItems();
		    							cb.addItem("Select All");
		    							for(int k=0;k<cb.getParentClass().getAllItems().size();k++){
		    								cb.addItem(cb.getParentClass().getAllItems().get(k));
		    							}
		    						}
		    									
		    						allSelected=true;
		    						if(allSelected){        					
		    	    					selected=all;
		    	    				}
		    				
		    						setChosenMolecules(selected); 
		    						int s=selected.size();
		    	    				setChosenMolecules(new ArrayList(selected)); 
		    				
		    	    				
		    	    				cb.setPopupVisible(false);
		    	    				cb.getBTip().setVisible(false);
		    	    				cb.getBTip().setEnabled(false);		    	    				
		    	    				cb.getParentClass().desellectAll(); 
		    	    				cb.getParentClass().getSelected().clear();
		    	    				cb.getParentClass().setFinished(false);
		    						break;
		    					
		    					
		    					}else{
		    						
		    						    	
		            		        CustomIconedCellRenderer rdr=cb.getParentClass().getRenderer();    		      
		            		        Vector selectedObjectsVector=rdr.getSelectedObjects();
		            		        int ss=selectedObjectsVector.size();	    	    				
		    				
		    						selected.clear();
		            		        for(int j=0;j<selectedObjectsVector.size();j++){
		            		        	
		            		        	Object o=selectedObjectsVector.get(j);
		            		        	selected.add(o);
		            		        	cb.removeItem(o);
		            		        }     
		            		        int s=selected.size();
		    	    				setChosenMolecules(new ArrayList(selected)); 		    					
		    	    				cb.setPopupVisible(false);
		    	    				cb.getBTip().setVisible(false);
		    	    				cb.getBTip().setEnabled(false);
		    	    				cb.getParentClass().desellectAll();   	    				
		    	    			
		    	    				cb.getParentClass().getSelected().clear();
		    	    				cb.getParentClass().setFinished(false);
		    	    				
		    					}
		    				}
		    				 				
		    				
						} 
						cb.getBTip().setVisible(false);
						cb.getBTip().setEnabled(false);				
					}
				}
			
			});
			
		
			main.add(cb);
			
			JButton ok=new JButton("Ok");
			ok.addActionListener(new ActionListener(){

				
				public void actionPerformed(ActionEvent e) {
					
					if(getChosenMolecules()!=null){
						String name="";
						for(int i=0;i<getChosenMolecules().size();i++){
							name+=getChosenMolecules().get(i);
							if(i<getChosenMolecules().size()-1){
								name+="-";
							}
						}
						nameTF.setText(name);
					}			
					
					setVisible(false);
			        dispose();
					
				}});			
			JButton cancel=new JButton("Cancel");
			cancel.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {					
					
					setVisible(false);
			        dispose();
				}});
			main.add(ok);
			main.add(cancel);
			
			lay.putConstraint(SpringLayout.WEST, t, 7,SpringLayout.WEST, this);
			lay.putConstraint(SpringLayout.NORTH, t, 10, SpringLayout.NORTH, this);
			lay.putConstraint(SpringLayout.WEST, cb, 100,SpringLayout.WEST, this);
			lay.putConstraint(SpringLayout.NORTH, cb, 30, SpringLayout.SOUTH, t);
			lay.putConstraint(SpringLayout.WEST, ok, 99,SpringLayout.WEST, this);
			lay.putConstraint(SpringLayout.NORTH, ok, 30, SpringLayout.SOUTH, cb);
			lay.putConstraint(SpringLayout.WEST, cancel, 10,SpringLayout.EAST, ok);
			lay.putConstraint(SpringLayout.NORTH, cancel, 0, SpringLayout.NORTH, ok);
			add(main,BorderLayout.CENTER);
			setResizable(true);
			setSize(350,180);
			setVisible(true);
						
		}

		public ArrayList<String> getChosenMolecules() {
			return chosenMolecules;
		}

		public void setChosenMolecules(ArrayList<String> chosenMolecules) {
			this.chosenMolecules = chosenMolecules;
		}
		
	}
}
