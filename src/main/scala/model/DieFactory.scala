package model

object DieFactory:
    def plain: Die =
        Die(bonusType = BonusType.None, bonusValue = 0)

    def chips(value: Int): Die =
        Die(bonusType = BonusType.Chips, bonusValue = value)

    def mult(value: Int): Die =
        Die(bonusType = BonusType.Mult, bonusValue = value)