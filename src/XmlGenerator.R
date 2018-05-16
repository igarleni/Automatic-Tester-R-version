library(xml2)

#' @title XmlGenerator_generateXml
#' @description function that creates a XML in a folder.
#' @import xml2
#' 
#' @param xmlRootName: name of the xml root node.
#' @param branchesData: data to be inserted on the xml, and its branches
#' @param filePath: path to the folder where this xml should be saved
#' 
#' @author Italo Garleni
#' 
XmlGenerator_generateXml = function(xmlRootName, branchesData, filePath)
{
  # Create XML with row data
  xmlRoot = xml_new_root(xmlRootName)
  variablesData = names(branchesData)
  for (variable in variablesData)
  {
    branchNodes = unlist(strsplit(variable,"[.]"))
    XmlGenerator_addBranch(xmlRoot, branchNodes, 
                           as.character(branchesData[,variable]))
  }
  
  # Save XML on project's config folder and its corresponding modelsBatteryPath 
  write_xml(xmlRoot, file = paste0(filePath, xmlRootName, ".xml"), options = c("no_empty_tags","format","no_declaration"))
}
#' @name XmlGenerator_addBranch
#' @description Recursive function that add branches on an XML root
#' @import xml2
#' 
#' @param father: root node where the branch should be added.
#' @param branch: vector of branch's nodes.
#' @param data: string with data that should be on branch's last node.
#' 
#' @note The data will be saved on this item (passed by reference)
#' 
#' @author Italo Garleni
#' 
XmlGenerator_addBranch <- function(father, branch, data)
{
  if (branch[1] %in% xml_name(xml_children(father)))
  {
    child = xml_child(father, branch[1])
    if (length(branch) < 2)
      xml_text(child) <- data
    else
      XmlGenerator_addBranch(child, branch[-1], data)
  }
  else
  {
    xml_add_child(father,branch[1])
    child = xml_child(father, branch[1])
    if (length(branch) < 2)
      xml_text(child) <- data
    else
      XmlGenerator_addBranch(child, branch[-1], data)
  }
}
