import com.lib.DataManagment.{DataLoader, DataUpdater}
import com.lib.MenuSystem.{Menu, MenuElement}
import com.lib.Model.Writable

import scala.collection.mutable

object Main extends App {
  val dataManager = DataLoader.loadData("src/resources/users.csv", "src/resources/units.csv")

  MenuElement.dataManager = dataManager
  MenuElement.current = com.lib.MenuSystem.Logging.WelcomeMenuElement

  Menu.start()

  DataUpdater.update(dataManager.users.asInstanceOf[mutable.Seq[Writable]], "src/resources/users.csv")
  DataUpdater.update(dataManager.units.asInstanceOf[mutable.Seq[Writable]], "src/resources/units.csv")
}