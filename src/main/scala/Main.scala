import com.lib.DataManagment.{DataLoader, DataUpdater}
import com.lib.MenuSystem.{ExitMenuElement, MenuElement}

object Main extends App {
  val dataManager = DataLoader.loadData("src/resources/users.csv", "src/resources/units.csv")

  MenuElement.dataManager = dataManager
  MenuElement.current = com.lib.MenuSystem.Logging.WelcomeMenuElement

  while (MenuElement.current != ExitMenuElement) {
    MenuElement.current.enter()
    MenuElement.current.update()
    print("\u001b[2J")
  }

  DataUpdater.update(dataManager.getUsers, "src/resources/users.csv")
  DataUpdater.update(dataManager.getUnits, "src/resources/units.csv")
}