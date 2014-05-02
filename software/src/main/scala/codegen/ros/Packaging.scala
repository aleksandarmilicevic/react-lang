package react.codegen.ros

/*

catkin compliant package.xml

*/

class PkgDescr(val name: String) {
  var version = ""
  var description = ""
  var author_email = "you@yourdomain.tld"
  var author_name = "John Doe"
  var maintainer_email = "you@yourdomain.tld"
  var maintainer_name = "John Doe"
  var license = "BSD"
  var dependencies = List("roscpp", "rospy", "std_msgs")
  var build_dependencies = List()
  var run_dependencies = List()
  var test_dependencies = List()
}

object Packaging {

  def catkinPkgXml(pkg: PkgDescr) = {
    var bdep = for(dep <- pkg.dependencies ++ pkg.build_dependencies)
      yield "<build_depend>" + dep + "</build_depend>"
    var rdep = for(dep <- pkg.dependencies ++ pkg.run_dependencies)
      yield "<run_depend>" + dep + "</run_depend>"
    var tdep = for(dep <- pkg.test_dependencies)
      yield "<test_depend>" + dep + "</test_depend>"
"""
<?xml version="1.0"?>
<package>
  <name>""" + pkg.name + """</name>
  <version>""" + pkg.version + """</version>
  <description>""" + pkg.description + """</description>

  <maintainer email=\"""" + pkg.maintainer_email +"""\">""" + pkg.maintainer_name + """</maintainer>
  <license>BSD</license>
  <url type="website">http://wiki.ros.org/beginner_tutorials</url>
  <author email=\"""" + pkg.author_email + """\">""" + pkg.author_email + """</author>

  <buildtool_depend>catkin</buildtool_depend>
""" +
    bdep.mkString("\n") +
    "\n" +
    rdep.mkString("\n") +
    "\n" +
    tdep.mkString("\n") +
"""
</package>
"""
  }

}
