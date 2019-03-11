# Proyectos
Repositorios de Proyectos

      TRAP
      IF       --- OPTION                     EQ     CHANGE MODE
      OR       --- INTERACTIVE PHASE          EQ     DATA MODIFICATION
T     NO INPUT CFA FAC1 FORMA DE VENTA        (AT APPEARANCE #    )
      *        Validar Tipo d Documento
      IF       CFA FAC1 TIPO DOCUMENTO        EQ
T     BLINK    CFA FAC1 TIPO DOCUMENTO        (AT APPEARANCE #    )
T     ERROR    << Favor Ingrese un tipo de Documento Valido >>
T     END
      *        Validar Forma de Pago en Blanco
      IF       CFA FACTURA1 FORMA DE PAGO     EQ
T     BLINK    CFA FACTURA1 FORMA DE PAGO     (AT APPEARANCE #    )
T     ERROR    << Favor Ingresar Forma de Pago >>
T     END
      *        Validar el Numero de Vendedor en Blanco
      IF       CFA FAC1 CODIGO VENDEDOR       EQ
T     BLINK    CFA FAC1 CODIGO VENDEDOR       (AT APPEARANCE #    )
T     ERROR    << Favor Ingrese un Numero de Vendedor >>
T     END
      *        Validar el Numero de Vendedor en Cotizacion2
      IF       CFA FAC1 CODIGO VENDEDOR C     EQ
T     BLINK    CFA FAC1 CODIGO VENDEDOR C     (AT APPEARANCE #    )
T     ERROR    << Favor Ingrese un Numero de Vendedor >>
T     END
      *        Validar que este ACtivo
      SET      CFA VENDEDOR CODIGO            =  CFA FAC1 CODIGO VENDEDOR C
      READ     CFA VENDEDOR               HOLD 0 FT 0 BY VENDEDOR CODIGO
T     IF       CFA VENDEDOR ACTIVO            EQ     N
TT    BLINK    CFA FAC1 CODIGO VENDEDOR C     (AT APPEARANCE #    )
TT    ERROR    << Vendedor Inactivo, Verifique >>
      *
      *        Validar que el Tipo de Documento no sea NOTA DE DEBITO
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     3
T     BLINK    CFA FAC1 TIPO DOCUMENTO        (AT APPEARANCE #    )
T     ERROR    << Este Tipo de Documento no es Permitio >>
T     END
      *        Validar Tipo de Transacion en blanco
      IF       CFA FAC1 TIPO TRANSACCION      LE     0
T     BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
T     ERROR    <<Favor Ingrese un tipo de Transaccion Valida >>
T     END
      *        Validar Numero de Cliente en Blanco
      IF       CFA FAC1 CODIGO CLIENTE        LE     0
T     BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
T     ERROR    << Favor Realize la Busqueda de un Cliente >>
T     END
      *        Activar Campo de No. de Recibo
      IF       --- DATABASE                   EQ     ESA
      OR       --- DATABASE                   EQ     PRU
T     IF       CFA FACTURA1 FORMA DE PAGO     EQ     N/CREDITO
T     AND      CFA FAC1 TIPO TRANSACCION      EQ     2
T     AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
T-    PASS         NRECIBO                    FIELD            SHARE? N
TT    GOSUB    --- .WIDGET SHOW BY NAME
TF    GOSUB    --- .WIDGET HIDE BY NAME
      *        Validar Clientes y proyectos de Pedido directo==================
      IF       CFA FAC1 FORMA DE VENTA        NE     1
      AND      --- DATABASE                   EQ     PRU
T     PASS         PROYECTO                   FIELD            SHARE? N
T     GOSUB    --- .WIDGET SHOW BY NAME
      *        Si el N° de Proyecto esta en Blanco.Fact imediata
T     IF       CFA FAC1 NO DE PROYECTO        EQ
TT    BLINK    CFA FAC1 NO DE PROYECTO        (AT APPEARANCE #    )
TT    WARNING  << Pedido Directo Sin Numero de Proyecto >>
      *        leer nombre del proyecto
TF    SET      WMS OUTPROYE PROYECTO ID       =  CFA FAC1 NO DE PROYECTO
TF    READ     WMS OUTPROYE               HOLD 0 FT 0 BY OUTPROYE PROYECTO ID
      *        Validar que el N° de Cliente sea el mismo que el del Proyecto
TFT   IF       WMS OUTPROYE NO CLIENTE        EQ CFA FAC1 CODIGO CLIENTE
TFTT  SET      CFA FAC1 NOMBRE PROYECTO       =  WMS OUTPROYE REFERENCIA
TFTT  DISPLAY  CFA FAC1 NOMBRE PROYECTO       (AT APPEARANCE #    )
      *        Si el cliente no es el mismo
TFTF  BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
TFTF  ERROR    << Este Proyecto no Pertenece a este Cliente, Verificar >>
TFTF  END
      *        Si no Existe el N° de Proyecto
TFF   BLINK    CFA FAC1 NO DE PROYECTO        (AT APPEARANCE #    )
TFF   ERROR    << N° de Proyecto no Existe Favor Verificar >>
TFF   END
      *        Si no es Pedido Directo no mostrar el campo de N° de Proyecto
F     PASS         PROYECTO                   FIELD            SHARE? N
F     GOSUB    --- .WIDGET HIDE BY NAME
      *        FINAL-==========================================================
      IF       --- DATABASE                   EQ     PRU
T     IF       CFA FAC1 CODIGO CLIENTE        NE     99999
TT    SET      CFA WORK CLIENTE               =  CFA FAC1 CODIGO CLIENTE
TT    SET      CFA W_NOMBRE_CLIENTE           =
TT    SET      CFA W_TELEFONOS_CLIENTES       =
TT    SET      CFA W_CELULCAR_CLIENTES        =
TT    GOSUB    CFA VERIFICAR Y EXTRAER CLIENTES
*TT   IF       CFA COT1 TEL                   EQ
*TTT  SET      CFA COT1 TEL                   =  CFA W_TELEFONOS_CLIENTES
*TT   IF       CFA COT1 FAX                   EQ
*TTT  SET      CFA COT1 FAX                   =  CFA W_CELULCAR_CLIENTES
TT    SET      CFA FAC1 NOMBRE CLIENTE        =  CFA W_NOMBRE_CLIENTE
TT    DISPLAY  CFA FAC1 NOMBRE CLIENTE        (AT APPEARANCE #    )
      *        si el cliente es Credito 99991
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99991
T     PASS         NEMPLEA                    FIELD            SHARE? N
T     GOSUB    --- .WIDGET SHOW BY NAME
T     IF       CFA FAC1 NO EMPLEADO           EQ
TT    BLINK    CFA FAC1 NO EMPLEADO           (AT APPEARANCE #    )
TT    ERROR    << Ingresar numero de Empleado >>
TT    END
      *        si el cliente no es 99991 ocultarlo y dejarlo en blanco
      IF       CFA FAC1 CODIGO CLIENTE        NE     99991
T     SET      CFA FAC1 NO EMPLEADO           =
T     PASS         NEMPLEA                    FIELD            SHARE? N
T     GOSUB    --- .WIDGET HIDE BY NAME
      *        Mostrar Saldo de Clientes Creditos
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
T     SET      CAR CUSTOMER NO                =  CFA FAC1 CODIGO CLIENTE
T     READ     CAR CUSTOMER               HOLD 0 FT 0 BY CUSTOMER NO
TT    SET      --- W                          =      0.00
TT    IF       CAR CUSTOMER NO                NE     99999
TTT   BEG LOOP L  = 001 TO 005  STEP 001
TTT   COMPUTE  --- W                          +  CAR CUSTOMER MOROSIDAD     L
TTT   END LOOP L
TTT   SET      CAR WORK AVAILABLE CREDIT      =  CAR CUSTOMER CREDIT LIMIT
TTT   COMPUTE  CAR WORK AVAILABLE CREDIT      -  --- W
TTT   DISPLAY  CAR WORK AVAILABLE CREDIT      (AT APPEARANCE #    )
      *
F     SET      CAR WORK AVAILABLE CREDIT      =      0.00
F     DISPLAY  CAR WORK AVAILABLE CREDIT      (AT APPEARANCE #    )
      *
      IF       CAR WORK AVAILABLE CREDIT      GT     0
T     SET      --- WIDGET NAME                =      SALDO
T     READ     --- WIDGET                 HOLD 1 FT 0 BY WIDGET NAME
TT    SET      --- WIDGET FONT BOLD           =      Y
TT    SET      --- WIDGET COLOR EN FG B       =      78
TT    SET      --- WIDGET COLOR EN FG G       =      199
TT    SET      --- WIDGET COLOR EN FG NL      =      1
TT    SET      --- WIDGET COLOR EN FG R       =      27
TT    REWRITE  --- WIDGET                 FAIL 0
      *
      IF       CAR WORK AVAILABLE CREDIT      LT     0
T     SET      --- WIDGET NAME                =      SALDO
T     READ     --- WIDGET                 HOLD 1 FT 0 BY WIDGET NAME
TT    SET      --- WIDGET FONT BOLD           =      Y
TT    SET      --- WIDGET COLOR EN FG B       =      0
TT    SET      --- WIDGET COLOR EN FG G       =      0
TT    SET      --- WIDGET COLOR EN FG NL      =      1
TT    SET      --- WIDGET COLOR EN FG R       =      255
TT    REWRITE  --- WIDGET                 FAIL 0
      *        Validar los departamentos por productos
      IF       --- INTERACTIVE PHASE          EQ     DATA MODIFICATION
T     SET      CFA FAC2 NO DOCUMENTO          =  CFA FAC1 NO DOCUMENTO
T     BEG AT   CFA FACTURA2 IN CFA FAC2 NO DOCUMENTO
T     END AT   CFA FACTURA2 IN CFA FAC2 NO DOCUMENTO
T     BEG READ CFA FACTURA2               HOLD 0 KEY IS  FAC2 NO DOCUMENTO
T     SET      CIC PRODUCT NO                 =  CFA FAC2 CODIGO PRODUCTO
T     READ     CIC PRODUCT                HOLD 0 FT 0 BY PRODUCT NO
      *
TT    IF       CIC PRODUCT NO                 EQ     P-D
TTT   GOTO     :SALTAR VALIDACION DEP
      *        Departamento 1
TT    IF       CIC PRODUCT ABC                EQ     A
TT    OR       CIC PRODUCT ABC                EQ
TTT   SET      --- R                          =      1
TTT   IF       CFA FAC1 DEPARTAMENTO          NE --- R
TTTT  BLINK    CFA FAC1 DEPARTAMENTO          (AT APPEARANCE #    )
TTTT  ERROR    << verificar lineas en la factura son GENERAL >>
TTTT  END
      *        Departamento 2
TT    IF       CIC PRODUCT ABC                EQ     D
TTT   SET      --- R                          =      2
TTT   IF       CFA FAC1 DEPARTAMENTO          NE --- R
TTTT  BLINK    CFA FAC1 DEPARTAMENTO          (AT APPEARANCE #    )
TTTT  ERROR    << verificar lineas en la factura son ESPECIAL >>
TTTT  END
      *        Departamento 3
TT    IF       CIC PRODUCT ABC                EQ     C
TTT   SET      --- R                          =      3
TTT   IF       CFA FAC1 DEPARTAMENTO          NE --- R
TTTT  BLINK    CFA FAC1 DEPARTAMENTO          (AT APPEARANCE #    )
TTTT  ERROR    << verificar lineas en la factura son M/Tension >>
TTTT  END
      *        SALTAR VALIDACION DEP
      LABEL    :SALTAR VALIDACION DEP
      *        Ernesto 14-06-2018
      *        Validar Beaker de claves P en venta General
TT    IF       CIC PRODUCT SUBCLASE           EQ     50.06
TT    AND      CFA FAC1 FORMA DE VENTA        NE     2
TTT   GOTO     SALTAR CLAVES P EN VTA 1 Y 3
      *        ------------------------------------------------------
      *        Validar forma de venta si P-D
TT    IF       CIC PRODUCT NO                 EQ     P-D
TTT   SET      --- S                          =      2
TTT   IF       CFA FAC1 FORMA DE VENTA        NE --- S
TTTT  BLINK    CFA FAC1 FORMA DE VENTA        (AT APPEARANCE #    )
TTTT  ERROR    << Documento con Lineas de Pedido directo P-D >>
TTTT  END
      *        Validar forma de venta si P-000/P-999
TT    IF       CIC PRODUCT NO                 GE     P-000
TT    AND      CIC PRODUCT NO                 LE     P-999
TTT   SET      --- S                          =      3
TTT   IF       CFA FAC1 FORMA DE VENTA        NE --- S
TTTT  BLINK    CFA FAC1 FORMA DE VENTA        (AT APPEARANCE #    )
TTTT  ERROR    << Documentos con Lineas de Pedido Directo >>
TTTT  END
      *        Validar Material Regular de Inventario
TT    IF       CIC PRODUCT NO                 NE     P-D
TT    AND      CIC PRODUCT NO                 LT     P-000
TTT   SET      --- S                          =      1
TTT   IF       CFA FAC1 FORMA DE VENTA        NE --- S
TTTT  BLINK    CFA FAC1 FORMA DE VENTA        (AT APPEARANCE #    )
TTTT  ERROR    << Documentos Con Lineas de Inventario Regular >>
TTTT  END
      *
      LABEL    SALTAR CLAVES P EN VTA 1 Y 3
      *
T     END READ CFA FACTURA2
      *
      *
      *        Validar Tipo de Transacciones Mayores a las Existentes
      IF       CFA FAC1 TIPO TRANSACCION      GT     2
      OR       CFA FAC1 TIPO TRANSACCION      EQ
T     BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
T     ERROR    << Ingrese un tipo de Transaccion Valido >>
T     END
      *        Validar N/Credito Solo para Clientes CONTADO
      IF       CFA FAC1 TIPO TRANSACCION      NE     2
      AND      CFA FACTURA1 FORMA DE PAGO     EQ     N/CREDITO
T     BLINK    CFA FACTURA1 FORMA DE PAGO     (AT APPEARANCE #    )
T     ERROR    << Las NOTAS DE CREDITO son solo para clientes CONTADO >>
T     END
      *
      *        ================================================================
      *        Validar Ingresar numero de RECIBO
      IF       CFA FAC1 TIPO TRANSACCION      EQ     2
      AND      CFA FACTURA1 FORMA DE PAGO     EQ     N/CREDITO
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
      *        Alerta de Facturas Pagadas con N/Credito
T     IF       CFA FAC1 NO RECIBO             EQ
TT    WARNING  << Esta Factura la Esta Pagando Con un N/Credito >>
      *        Validar Numero de Recibo que Exista
T     IF       CFA FAC1 NO RECIBO             NE
TT    SET      CFA NCREDITO NO N-CREDITO      =  CFA FAC1 NO RECIBO
TT    READ     CFA NCREDITO               HOLD 0 FT 0 BY NCREDITO NO N-CREDITO
TTF   BLINK    CFA FAC1 NO RECIBO             (AT APPEARANCE #    )
TTF   ERROR    << Este numero de Recibo no Existe, Verificar!!! >>
TTF   END
      *        ================================================================
      *        Validar tipo de Entrega
      IF       CFA FAC1 TIPO ENTREGA          EQ
      OR       CFA FAC1 TIPO ENTREGA          GE     5
T     BLINK    CFA FAC1 TIPO ENTREGA          (AT APPEARANCE #    )
T     ERROR    << Favor Ingresar un tipo de Entrega Valido >>
T     END
      *        Validar campos de Entrega Para Pedidos Mostrador
      IF       CFA FAC1 TIPO ENTREGA          EQ     1
T     OK INPUT CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
T     OK INPUT CFA FACTURA1 ENTREGAR A        (AT APPEARANCE #    )
T     OK INPUT CFA FACTURA1 ENTREGAR EN       (AT APPEARANCE #    )
      *        ----Crear Fecha si esta en Blanco
T     IF       CFA FAC1 FECHA ENTREGA         EQ
TT    SET DATE CFA FAC1 FECHA ENTREGA
      *
T     DATE BTW CFA FAC1 FECHA TRANSACCION     5  CFA FAC1 FECHA ENTREGA
T     IF       --- BETWEEN                    LT     0
TT    BLINK    CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
TT    ERROR    << la FECHA de entrega es MENOR al dia actual, VERIFICAR !!! >>
TT    END
      *        ----Validar FACTURA1 ENTREGAR A
      *        Activar Campos de Entrega Para Pedido de Envio
      IF       CFA FAC1 TIPO ENTREGA          EQ     2
T     OK INPUT CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
T     OK INPUT CFA FACTURA1 ENTREGAR A        (AT APPEARANCE #    )
T     OK INPUT CFA FACTURA1 ENTREGAR EN       (AT APPEARANCE #    )
      *        ----Validar Fecha de Entrega
T     IF       CFA FAC1 FECHA ENTREGA         EQ
T     OR       CFA FAC1 FECHA ENTREGA         LT CFA PARAM FECHA CONTROL
TT    BLINK    CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
TT    ERROR    << Ingresar Fecha de Entrega >>
TT    END
      *
T     DATE BTW CFA FAC1 FECHA TRANSACCION     5  CFA FAC1 FECHA ENTREGA
T     IF       --- BETWEEN                    LT     0
TT    BLINK    CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
TT    ERROR    << la FECHA de entrega es MENOR al dia actual, VERIFICAR !!! >>
TT    END
      *        ----Validar FACTURA1 ENTREGAR A
T     IF       CFA FACTURA1 ENTREGAR A        EQ
TT    BLINK    CFA FACTURA1 ENTREGAR A        (AT APPEARANCE #    )
TT    ERROR    << Favor Ingresar Persona que Recibe >>
TT    END
      *        ----Validar FACTURA1 ENTREGAR EN
T     IF       CFA FACTURA1 ENTREGAR EN       EQ
TT    BLINK    CFA FACTURA1 ENTREGAR EN       (AT APPEARANCE #    )
TT    ERROR    << Favor Ingresar una direccion Valida >>
TT    END
      *
      *        VAlidar Fecha de entrega para el tipo Preparar Material
      IF       CFA FAC1 TIPO ENTREGA          EQ     3
T     OK INPUT CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
T     IF       CFA FAC1 FECHA ENTREGA         EQ
TT    BLINK    CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
TT    ERROR    << Ingresar Fecha para Preparar >>
TT    END
      *
T     DATE BTW CFA FAC1 FECHA TRANSACCION     5  CFA FAC1 FECHA ENTREGA
T     IF       --- BETWEEN                    LT     0
TT    BLINK    CFA FAC1 FECHA ENTREGA         (AT APPEARANCE #    )
TT    ERROR    << la FECHA de entrega es MENOR al dia actual, VERIFICAR !!! >>
TT    END
      *
      *        Validar Clientes
      IF       CFA FAC1 CODIGO CLIENTE        NE     0
T     IF       CFA FAC1 NOMBRE CLIENTE        EQ
TT    BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
TT    ERROR    HAGA LA BUSQUEDA CON F2 DEL CLIENTE EN LA TABLA Q CORRESPONDA
TT    END
      *
      *
      IF       CFA FAC1 FORMA DE VENTA        EQ     2
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
T     IF       CFA FACTURA1 FORMA DE PAGO     EQ     CREDITO
TT    IF       CFA FAC1 REFERN CREDITO        EQ
TTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TTT   ERROR    Todo Pedido Directo necesita APROBACION para proceder a facturar
      *
*     IF       CFA FAC1 FORMA DE VENTA        EQ     2
*     AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
*     AND      CFA FAC1 NO BODEGA             EQ     6
*T    IF       CFA FACTURA1 FORMA DE PAGO     EQ     CREDITO
*TT   IF       CFA FAC1 REFERN CREDITO        EQ
*TTT  BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
*TTT  ERROR    Todo Pedido Directo necesita APROBACION para proceder a facturar
      *
      *
      IF       CFA FACTURA1 FORMA DE PAGO     NE     CREDITO
T     IF       CFA FAC1 CODIGO CLIENTE        EQ     99999
TF    SET      CFA CONTADO NO                 =  CFA FAC1 CODIGO CLIENTE
TF    READ     CFA CONTADO                HOLD 0 FT 0 BY CONTADO NO
TFF   ERROR    Este Cliente NO existe en la Tabla Contado ... Verifique !!!
TFF   END
      *
T     IF       CFA FAC1 CODIGO CLIENTE        NE     99999
TT    SET      CFA FAC1 NOMBRE CLIENTE        =  CFA CONTADO NAME
TT    DISPLAY  CFA FAC1 NOMBRE CLIENTE        (AT APPEARANCE #    )
F     SET      CAR CUSTOMER NO                =  CFA FAC1 CODIGO CLIENTE
F     READ     CAR CUSTOMER               HOLD 0 FT 0 BY CUSTOMER NO
FF    ERROR    Este Cliente NO existe en la Tabla de Clientes Creditos  !!!
FF    END
F     IF       CFA FAC1 CODIGO CLIENTE        NE     99999
FT    SET      CFA FAC1 NOMBRE CLIENTE        =  CAR CUSTOMER NAME
FT    DISPLAY  CFA FAC1 NOMBRE CLIENTE        (AT APPEARANCE #    )
      *
      *        Nueva VALIDACION 13/05/2015
      *        La forma de Pago CREDITO NO PUEDE APLICAR A UN CLIENTE CONTADO
      *
      *        Validamos Nota Creditos aplicado a codigo 99999 cuya forma de
      *        pago sea CREDITO
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     02
      AND      CFA FACTURA1 FORMA PAGO        EQ     CREDITO
      AND      CFA FAC1 CODIGO CLIENTE        EQ     99999
T     ERROR    Verifique Forma de Pago para esta Devolucion !!!
      *
      SET      CAR DIV DIVISION NO            =  CAR CUSTOMER DIVISION
      READ     CAR DIV                    HOLD 0 FT 0 BY DIV DIVISION NO
      *
      *
      *        Validar Vendedores Activos
      SET      CFA VENDEDOR CODIGO            =  CFA FAC1 CODIGO VENDEDOR
      READ     CFA VENDEDOR               HOLD 0 FT 0 BY VENDEDOR CODIGO
T     IF       CFA VENDEDOR ACTIVO            EQ     N
TT    BLINK    CFA FAC1 CODIGO VENDEDOR       (AT APPEARANCE #    )
TT    ERROR    << Codigo de Vendedor Desactivado, Verifique!!! >>
TT    END
      *
      IF       CFA FAC1 CODIGO VENDEDOR       EQ     3
      AND      --- USER ID                    NE     ECB
T     ERROR    Verifique el numero de Vendedor que esta utilizando !!!
      *
      IF       CFA FAC1 NO BODEGA             EQ     1
      AND      CAR CUSTOMER COMPETENCIA       EQ     1
*     AND      CFA VENDEDOR TIPO DE PRECI 005 EQ     1
T     IF       --- USER ID                    EQ     EYM
T     OR       --- USER ID                    EQ     IYM
T     OR       --- USER ID                    EQ     EAM
T     OR       --- USER ID                    EQ     IAR
T     OR       --- USER ID                    EQ     EJJ
TF    ERROR    Este Cliente debe ser Atendido por Ilka o Edgar Favor Remitirlo
      *
      IF       CFA FAC1 NO BODEGA             EQ     5
      AND      CAR CUSTOMER COMPETENCIA       EQ     1
T     IF       --- USER ID                    EQ     IAR
T     OR       --- USER ID                    EQ     SPA
T     OR       --- USER ID                    EQ     AMV
T     OR       --- USER ID                    EQ     WGB
TF    ERROR    Este Cliente debe ser Atendido por Sarahi o Isabel Favor Remitir
      *
      *
      *        Nueva Validacion de Vendedores por Sucursal--2017-EAM
      SET      CFA VENDXSUC SUCURSAL          =  CFA FAC1 NO BODEGA
      SET      CFA VENDXSUC NO VENDEDOR       =  CFA FAC1 CODIGO VENDEDOR
      READ     CFA VENDXSUC               HOLD 0 FT 0 BY VENDXSUC KEY
      *        Validar Vendedores q no son de Betania
F     IF       CFA FAC1 NO BODEGA             EQ     1
FT    ERROR    <<N° de Vendedor no autorizado para Facturar en Betania>>
FT    END
      *        Validar Vendedores q no son de Incorsa
F     IF       CFA FAC1 NO BODEGA             EQ     5
FT    ERROR    <<N° de Vendedor no autorizado para Facturar en Incorsa>>
FT    END
      *        Validar Vendedores q no son de David
F     IF       CFA FAC1 NO BODEGA             EQ     2
FT    ERROR    <<N° de Vendedor no autorizado para Facturar en David>>
FT    END
      *        Validar Vendedores q no son de Chitre
F     IF       CFA FAC1 NO BODEGA             EQ     4
FT    ERROR    <<N° de Vendedor no autorizado para Facturar en Chitre>>
FT    END
      *        Validar Vendedores q no son de Pedido Directo
F     IF       CFA FAC1 NO BODEGA             EQ     6
FT    ERROR    <<N° de Vendedor no autorizado para Facturar en P-Directo>>
FT    END
      *
      *        Nueva Validacion de ID de Usuarios por Sucursal
      SET      CFA USERDEP USER ID            =  --- USER ID
      SET      CFA USERDEP DEPOSITO           =  CFA FAC1 NO BODEGA
      READ     CFA USERDEP                HOLD 0 FT 0 BY USERDEP LLAVE
      *        Validar Vendedores q no son de Betania
F     IF       CFA FAC1 NO BODEGA             EQ     1
FT    ERROR    <<Usuario no autorizado para Facturar en Betania>>
FT    END
      *        Validar Vendedores q no son de Incorsa
F     IF       CFA FAC1 NO BODEGA             EQ     5
FT    ERROR    <<Usuario no autorizado para Facturar en Incorsa>>
FT    END
      *        Validar Vendedores q no son de David
F     IF       CFA FAC1 NO BODEGA             EQ     2
FT    ERROR    <<Usuario no autorizado para Facturar en David>>
FT    END
      *        Validar Vendedores q no son de Chitre
F     IF       CFA FAC1 NO BODEGA             EQ     4
FT    ERROR    <<Usuario no autorizado para Facturar en Chitre>>
FT    END
      *        Validar Vendedores q no son de Pedido Directo
F     IF       CFA FAC1 NO BODEGA             EQ     6
FT    ERROR    <<Usuario no autorizado para Facturar en P-Directo>>
FT    END
      *        *
      *
      *        Validar si el usuario esta en la Tabla Fiscal
      *
      SET      CFA WORK INDICADOR CAJA        =      N
      SET      CFA WORK ERROR EN USERDEP      =      N
      SET      CFA USERDEP USER ID            =  --- USER ID
      SET      CFA USERDEP DEPOSITO           =  CFA FAC1 NO BODEGA
      READ     CFA USERDEP                HOLD 0 FT 0 BY USERDEP LLAVE
F     SET      CFA WORK ERROR EN USERDEP      =      Y
F     ERROR    Este usuario no esta creado en la Tabla Fiscal
T     IF       --- DATABASE                   EQ CFA USERDEP BASE DATOS
TF    SET      CFA WORK ERROR EN USERDEP      =      Y
TF    ERROR    El usuario no esta registrasdo en la Base de datos Fiscal.
      *        ***************************************************************
      TRAP
      SET      CFA CONTCAJA BASE DATOS        =  --- DATABASE
      SET      CFA CONTCAJA DEPOSITO          =  CFA USERDEP DEPOSITO
      READ     CFA CONTCAJA               HOLD 0 FT 0 BY CONTCAJA LLAVE
F     GOTO     NO CAJADEP1
T     IF       CFA CONTCAJA ACTIVADO          EQ     N
TT    GOTO     NO CAJADEP1
      *
      IF       CFA FACTURA1 NO CAJA           EQ     0
      AND      CFA WORK ERROR EN USERDEP      EQ     N
T     SET      CFA WORK NO CAJA ASIGNA        =      0
T     SET      CFA WORK SECUENCIA CAJA        =      0
T     SET      CFA WORK USER CAJERO           =
T     SET      CFA CAJADEP NO CAJA            =      1
T     BEG AT   CFA CAJADEP  IN CFA CAJADEP NO CAJA
T     SET      CFA CAJADEP NO CAJA            =      99
T     END AT   CFA CAJADEP  IN CFA CAJADEP NO CAJA
T     BEG READ CFA CAJADEP                HOLD 0 KEY IS  CAJADEP NO CAJA
T     IF       CFA USERDEP DEPOSITO           EQ CFA CAJADEP DEPOSITO
TT    IF       CFA WORK INDICADOR CAJA        EQ     N
TTT   SET      CFA WORK INDICADOR CAJA        =      Y
TTT   SET      CFA WORK NO CAJA ASIGNA        =  CFA CAJADEP NO CAJA
TTT   SET      CFA WORK SECUENCIA CAJA        =  CFA CAJADEP CONTADOR
TTT   SET      CFA WORK USER CAJERO           =  CFA CAJADEP CAJERO ASIGNAS
TTF   IF       CFA CAJADEP CONTADOR           LT CFA WORK SECUENCIA CAJA
TTFT  SET      CFA WORK SECUENCIA CAJA        =  CFA CAJADEP CONTADOR
TTFT  SET      CFA WORK NO CAJA ASIGNA        =  CFA CAJADEP NO CAJA
TTFT  SET      CFA WORK USER CAJERO           =  CFA CAJADEP CAJERO ASIGNAS
T     END READ CFA CAJADEP
T     SET      CFA FACTURA1 NO CAJA           =  CFA WORK NO CAJA ASIGNA
T     SET      CFA CAJADEP BASE DATOS         =  --- DATABASE
T     SET      CFA CAJADEP DEPOSITO           =  CFA USERDEP DEPOSITO
T     SET      CFA CAJADEP NO CAJA            =  CFA WORK NO CAJA ASIGNA
T     READ     CFA CAJADEP                HOLD 1 FT 0 BY CAJADEP LLAVE
TT    COMPUTE  CFA CAJADEP CONTADOR           +      1
TT    REWRITE  CFA CAJADEP                FAIL 0
      *
      DISPLAY  CFA FACTURA1 NO CAJA           (AT APPEARANCE #    )
      *
      IF       CFA FACTURA1 FRELAC            EQ     Y
T     IF       CFA WORK ULTIMO NO CAJA        NE     0
TT    IF       CFA FACTURA1 NO CAJA           EQ CFA WORK ULTIMO NO CAJA
TTF   SET      CFA CAJADEP CAJERO ASIGNAS     =  CFA WORK USER CAJERO
TTF   READ     CFA CAJADEP                HOLD 1 FT 0 BY CAJADEP CAJERO ASIGNAS
TTFT  COMPUTE  CFA CAJADEP CONTADOR           -      1
TTFT  REWRITE  CFA CAJADEP                FAIL 0
TTF   SET      CFA CAJADEP BASE DATOS         =  --- DATABASE
TTF   SET      CFA CAJADEP DEPOSITO           =  CFA USERDEP DEPOSITO
TTF   SET      CFA CAJADEP NO CAJA            =  CFA WORK ULTIMO NO CAJA
TTF   READ     CFA CAJADEP                HOLD 1 FT 0 BY CAJADEP LLAVE
TTFT  COMPUTE  CFA CAJADEP CONTADOR           +      1
TTFT  REWRITE  CFA CAJADEP                FAIL 0
TT    SET      CFA FACTURA1 NO CAJA           =  CFA WORK ULTIMO NO CAJA
TT    SET      CFA WORK ULTIMO NO CAJA        =      0
TT    DISPLAY  CFA FACTURA1 NO CAJA           (AT APPEARANCE #    )
      *        ***************************************************************
      LABEL    NO CAJADEP1
      *        ***************************************************************
      **       DESDE AQUI
*     IF       CFA USERDEP DEPOSITO           EQ     1
*     AND      CFA WORK ERROR EN USERDEP      EQ     N
*T    IF       CFA FACTURA1 NO CAJA           EQ     0
*TT   BLINK    CFA FACTURA1 NO CAJA           (AT APPEARANCE #    )
*TT   ERROR    Indique Numero de Caja donde desea enviar la Orden  1 o 2
*TT   END
      *
*T    IF       CFA FACTURA1 NO CAJA           EQ     1
*T    OR       CFA FACTURA1 NO CAJA           EQ     2
*TF   ERROR    Numero de Caja Valido es 1 o 2 ... Verifique !!!
*TF   END
*F    SET      CFA FACTURA1 NO CAJA           =      0
      **       HASTA AQUI
      *
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 REFERN CREDITO        EQ     C0D
T     ERROR    Con Ud. Sr. Vendedor recuerde que es COD no invente por favor !!
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 REFERN CREDITO        EQ     C.O.D
T     ERROR    Insistes en invetar Codigos que no son recuerda es COD solo eso!
      *
      *
*     IF       CFA FAC1 NO BODEGA             EQ     1
*T    SET      CFA WORK SUCURSAL              =      ELECTRISA
*T    DISPLAY  CFA WORK SUCURSAL              (AT APPEARANCE #    )
      *
*     IF       CFA FAC1 NO BODEGA             EQ     5
*T    SET      CFA WORK SUCURSAL              =      INCORSA
*T    DISPLAY  CFA WORK SUCURSAL              (AT APPEARANCE #    )
      *
*     IF       CFA FAC1 NO BODEGA             EQ     6
*T    SET      CFA WORK SUCURSAL              =      PDIRECTO
*T    DISPLAY  CFA WORK SUCURSAL              (AT APPEARANCE #    )
      *
*     IF       CFA FAC1 NO BODEGA             EQ     0
*T    BLINK    CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    DISPLAY  CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    ERROR    Los depositos validos son 1 para ELECTRISA y 5 para INCORSA
      *
*     IF       CFA FAC1 NO BODEGA             EQ     2
*T    SET      CFA WORK SUCURSAL              =      S/DAVID
*T    BLINK    CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    DISPLAY  CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    ERROR    Los depositos validos son 1 para ELECTRISA y 5 para INCORSA
      *
      *
*     IF       CFA FAC1 NO BODEGA             EQ     3
*T    BLINK    CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    DISPLAY  CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    ERROR    Los depositos validos son 1 para ELECTRISA y 5 para INCORSA
      *
*     IF       CFA FAC1 NO BODEGA             EQ     4
*T    BLINK    CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    DISPLAY  CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    ERROR    Los depositos validos son 1 para ELECTRISA y 5 para INCORSA
      *
*     IF       CFA FAC1 NO BODEGA             GT     6
*T    BLINK    CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    DISPLAY  CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
*T    ERROR    Los depositos validos son 1 para C.M, 5 para INCORSA,6 PDIRECTO
      *
      *        ----------CREAR VALIDACION DE LA FACTURACION DE CLIENTES-------
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
T     IF       CFA FAC1 CODIGO CLIENTE        NE     99991
      *        Colocar la subrutina
TT    GOSUB    CFA VALIDAR SUC. DE CLIENTE CREDIT
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     2
T     IF       CFA FAC1 CODIGO CLIENTE        NE     99999
T     OR       CFA FAC1 CODIGO CLIENTE        NE     70005
      *        colocarl la subrutina
*TT   GOSUB    CFA VALIDAR SUC. DE CLIENTE CONTAD
      *
      *        ---------------------------------------------------------------
*BLN  IF       CFA FAC1 TIPO TRANSACCION      EQ     1
*BLN  AND      CAR CUSTOMER AREA              EQ     4
*T    ERROR    Este cliente esta asignado a Sucursal David, no a Electrisa Pal
      *
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
C     AND      CAR CUSTOMER AREA              EQ     6
CT    ERROR    Este cliente esta asignado a Sucursal Chitre,no a Electrisa Pal
      *
*BLN  IF       CFA FAC1 TIPO TRANSACCION      EQ     1
*BLN  OR       CFA FAC1 TIPO TRANSACCION      EQ     2
*T    IF       CFA FAC1 CODIGO CLIENTE        GE     6000
*T    AND      CFA FAC1 CODIGO CLIENTE        LE     6999
*TT   ERROR    Cliente asignado a Sucursal CHITRE, no a Electrisa Principal
      *
C     IF       CFA FAC1 CODIGO CLIENTE        EQ     99998
CT    ERROR    Este cliente ya no esta habilitado para su uso !!!!
      *
      *
      *
      *        Validar que solo el Vendedor 20 le Facture a ALTENSA
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99
T     IF       CFA FAC1 CODIGO VENDEDOR       EQ     20
TF    BLINK    CFA FAC1 CODIGO VENDEDOR       (AT APPEARANCE #    )
TF    BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
TF    ERROR    Este cliente esta asignado a vendedores autorizados......
      *
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 REFERN CREDITO        EQ     COD
T     BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
T     BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
T     ERROR    "El Tratamiento COD es solo para Documentos Contado"...Verifique
      *
      GOTO     NO VALIDAR VENDEDOR
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     3
      AND      CAR DIV DIVISION NO            EQ     11
T     IF       CAR CUSTOMER SALESPERSON       NE CFA FAC1 CODIGO VENDEDOR
TT    BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
TT    BLINK    CFA FAC1 CODIGO VENDEDOR       (AT APPEARANCE #    )
TT    ERROR    ESTE CLIENTE NO PERTENECE A ESTE VENDEDOR.....INTENTE NUEVAMENTE
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     2
      AND      CAR DIV DIVISION NO            EQ     11
T     IF       CAR CUSTOMER SALESPERSON       NE CFA FAC1 CODIGO VENDEDOR
TT    BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
TT    BLINK    CFA FAC1 CODIGO VENDEDOR       (AT APPEARANCE #    )
TT    ERROR    ESTE CLIENTE NO PERTENECE A ESTE VENDEDOR.....INTENTE NUEVAMENTE
      *
      *
      *
      LABEL    NO VALIDAR VENDEDOR
      *
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
C     AND      CAR DIV DIVISION NO            GE     10
CT    BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
CT    ERROR    ESTE CLIENTE ESTA ASIGNADO PARA VENTAS CONTADO ESPECIALES
      *
      IF       CFA WORK SW1                   EQ     1
      AND      CFA WORK SW                    EQ     1
T     SET      CFA FAC1 PORCENTAJE DESCTO     =
T     NO INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
T     DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
T     SET      CFA FAC1 PORCENTAJE DESCT1     =
T     NO INPUT CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
T     DISPLAY  CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      *
      IF       CFA WORK SW1                   EQ     1
T     SET      CFA FAC1 PORCENTAJE DESCTO     =
T     NO INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
T     DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
T     SET      CFA FAC1 PORCENTAJE DESCT1     =
T     NO INPUT CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
T     DISPLAY  CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      *
      *
      *
      *
C     TRAP
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
CT    IF       CFA FAC1 PORCENTAJE DESCTO     GT     0
CTT   IF       CFA WORK SW                    EQ     1
CTTT  BLINK    CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
CTTT  BLINK    CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
CTTT  ERROR    No puedes aplicar descuento si facturaste Ya precio Neto !!!!
      *
      *
C     TRAP
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     3
CT    IF       CFA FAC1 PORCENTAJE DESCT1     GT     0
CT    OR       CFA FAC1 PORCENTAJE DESCTO     GT     0
CTT   IF       CFA WORK SW                    EQ     1
CTTT  BLINK    CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
CTTT  BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
CTTT  ERROR    No debes aplicar descuento a la factura si otorgaste precio neto
      *
C     IF       CFA FAC1 PORCENTAJE DESCT1     GT     0
C     AND      CFA FAC1 NOMBRE 2              EQ
C     AND      CFA FAC1 CEDULA                EQ
CT    BLINK    CFA FAC1 NOMBRE 2              (AT APPEARANCE #    )
CT    BLINK    CFA FAC1 CEDULA                (AT APPEARANCE #    )
CT    ERROR    Informacion Necesaria cuando otorgamos descuento aparte
      *
C     IF       CFA FAC1 PORCENTAJE DESCT1     GT     0
C     AND      CFA FAC1 NOMBRE 2              EQ
CT    BLINK    CFA FAC1 NOMBRE 2              (AT APPEARANCE #    )
CT    ERROR    Registre el Nombre Completo a quien se le otorga el Descuento
      *
C     IF       CFA FAC1 PORCENTAJE DESCT1     GT     0
C     AND      CFA FAC1 CEDULA                EQ
CT    BLINK    CFA FAC1 CEDULA                (AT APPEARANCE #    )
CT    WARNING  Registre la cedula de la persona que se le otorga el Descuento
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
T     IF       CFA FAC1 CODIGO CLIENTE        EQ     99990
T     AND      CFA FAC1 REFERN CREDITO        EQ
TT    BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TT    ERROR    Las Ventas de Accionistas Necesitan Referencia del Dpto. Credito
      *
      *        Validamos los clientes Excentos para que no se calcule el ITBM
      *
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     2
      AND      CAR CUSTOMER EXCENTA           EQ     1
T     IF       CFA FAC1 CALCULO IMPUESTO      EQ     1
TT    ERROR    Recuerde que es un Cliente Excento... Asigne "N" en Impuesto
      *
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CAR CUSTOMER EXCENTA           EQ     1
T     IF       CFA FAC1 CALCULO IMPUESTO      EQ     1
TT    ERROR    Recuerde que es un Cliente Excento... Asigne "N" en Impuesto....
      *
      *
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
CT    IF       CAR CUSTOMER TYPE              NE     8
CT    AND      CAR CUSTOMER TYPE              NE     9
CTT   IF       CFA FAC1 CALCULO IMPUESTO      EQ     0
CTTT  ERROR    Este Cliente NO es Excento... Consulte al Depto Credito..
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 CALCULO IMPUESTO      EQ     0
T     IF       CAR CUSTOMER EXCENTA           NE     1
TT    ERROR    Este Cliente No es Excento del ITBM Consulte al Dpto. Credito
      *
      *        Validamos los Clientes de una morosidad a mas de 60 dias
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
T     IF       CAR CUSTOMER TERMS CODE        LE     60
T     AND      CAR CUSTOMER MOROSIDAD     003 GT     0
T     AND      CFA FAC1 REFERN CREDITO        EQ
TT    BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TT    ERROR    Cliente necesita APROBACION para proceder a facturar
      *
      *        Validamos los Clientes de una morosidad a mas de 30 dias
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
T     IF       CAR CUSTOMER TERMS CODE        EQ     30
T     AND      CAR CUSTOMER MOROSIDAD     002 GT     0
T     AND      CFA FAC1 REFERN CREDITO        EQ
TT    BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TT    ERROR    Cliente necesita APROBACION para proceder a facturar
      *
*     IF       CFA FAC1 TIPO TRANSACCION      EQ     6
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     1
      AND      CFA FAC1 FORMA DE VENTA        EQ     2
      AND      CFA FACTURA1 FORMA PAGO        EQ     CREDITO
T     IF       CAR CUSTOMER TERMS CODE        LE     60
T     AND      CAR CUSTOMER MOROSIDAD     003 GT     0
T     AND      CFA FAC1 REFERN CREDITO        EQ
TT    BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TT    ERROR    Cliente necesita APROBACION para proceder a facturar
      *
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
      AND      CAR CUSTOMER DUNNING CODE      EQ     1
T     ERROR    Cliente necesita autorizacion gerencial para proceder a Facturar
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 CODIGO CLIENTE        EQ     99992
T     ERROR    Este codigo ha sido asignado para ventas Contado Empleado
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     2
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
T     IF       CFA FAC1 CODIGO CLIENTE        EQ     99992
T     AND      CFA FAC1 REFERN CREDITO        EQ
TT    BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TT    ERROR    Ventas Contado a Colaboradores Requiere Clave... llame a Credito
      *
      *
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99992
      AND      CFA FAC1 PORCENTAJE DESCTO     GT     50
T     ERROR    Los descuentos a empleados no pueden ser mayor al 50%
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
T     IF       CFA FAC1 CODIGO CLIENTE        EQ     99991
T     AND      CFA FAC1 REFERN CREDITO        EQ
TT    BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TT    ERROR    Las Ventas a Empleados Necesitan Referencia del Dpto. de Credito
      *        Validar que sea vendido por Vendedor Especifico
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99991
      OR       CFA FAC1 CODIGO CLIENTE        EQ     70005
T     IF       --- USER ID                    EQ     IYM
T     OR       --- USER ID                    EQ     NYP
T     OR       --- USER ID                    EQ     EYM
T     OR       --- USER ID                    EQ     SPA
T     OR       --- USER ID                    EQ     JJE
T     OR       --- USER ID                    EQ     EAM
TF    BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
TF    ERROR    << Usted no Esta Autorizado Para Vender al Personal >>
TF    END
      *
      TRAP
      IF       CFA FAC1 DEPARTAMENTO          EQ     2
T     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
T     OR       CFA FAC1 TIPO TRANSACCION      EQ     2
TT    IF       --- P                          NE     1
TTT   SET      --- P                          =      0
TTT   SET      CAR DESCUENT CLIENTE           =  CFA FAC1 CODIGO CLIENTE
TTT   READ     CAR DESCUENT               HOLD 0 FT 0 BY DESCUENT CLIENTE
TTTT  SET      CFA FAC1 PORCENTAJE DESCTO     =  CAR DESCUENT PORCENTAJE
TTTT  SET      --- P                          =      1
TTTT  OK INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTTT  DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTT-  GOTO     NO VALIDAR DESCUENTO
      *
      *        Validamos ventas Contados con Descuentos especificos
      *
      IF       CFA FAC1 CODIGO CLIENTE        GE     7000
      AND      CFA FAC1 CODIGO CLIENTE        LE     8000
T     IF       CFA FAC1 TIPO TRANSACCION      EQ     2
T     AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
TT    SET      CAR DESCUENT CLIENTE           =  CFA FAC1 CODIGO CLIENTE
TT    READ     CAR DESCUENT               HOLD 0 FT 0 BY DESCUENT CLIENTE
TTT   SET      CFA FAC1 PORCENTAJE DESCTO     =  CAR DESCUENT PORCENTAJE
TTT   NO INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTT   DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTT   NO INPUT CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
TTT   DISPLAY  CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
TTT-  GOTO     NO VALIDAR DESCUENTO
      *
      *
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99992
T     IF       CFA FAC1 TIPO TRANSACCION      GE     2
T     OR       CFA FAC1 TIPO TRANSACCION      LE     3
TT    SET      CAR DESCUENT CLIENTE           =  CFA FAC1 CODIGO CLIENTE
TT    READ     CAR DESCUENT               HOLD 0 FT 0 BY DESCUENT CLIENTE
TTT   SET      CFA FAC1 PORCENTAJE DESCTO     =  CAR DESCUENT PORCENTAJE
TTT   NO INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTT   DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTT   NO INPUT CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
TTT   DISPLAY  CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
TTT-  GOTO     NO VALIDAR DESCUENTO
      *
      BEG LOOP EI = 001 TO 003  STEP 001
      *
      IF       CFA FAC1 DEPARTAMENTO          EQ --- EI
T     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
T     OR       CFA FAC1 TIPO TRANSACCION      EQ     2
T     OR       CFA FAC1 TIPO TRANSACCION      EQ     3
TT    SET      --- P                          =      0
TT    SET      CAR DESCUENT CLIENTE           =  CFA FAC1 CODIGO CLIENTE
TT    READ     CAR DESCUENT               HOLD 0 FT 0 BY DESCUENT CLIENTE
TTT   SET      --- P                          =      1
TTT   SET      CFA FAC1 PORCENTAJE DESCTO     =  CAR DESCUENT PORCENTAJE
TTT   NO INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTT   DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTT   IF       CAR DESCUENT ORDEN SI O NO     EQ     1
TTTT  SET      CFA FAC1 NUMERO ORDEN          =  CAR DESCUENT ORDEN COMPRA
TTTT  NO INPUT CFA FAC1 NUMERO ORDEN          (AT APPEARANCE #    )
TTTT  DISPLAY  CFA FAC1 NUMERO ORDEN          (AT APPEARANCE #    )
TTTF  OK INPUT CFA FAC1 NUMERO ORDEN          (AT APPEARANCE #    )
TTTF  DISPLAY  CFA FAC1 NUMERO ORDEN          (AT APPEARANCE #    )
TTT-  GOTO     NO VALIDAR DESCUENTO
TTF   OK INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTF   OK INPUT CFA FAC1 NUMERO ORDEN          (AT APPEARANCE #    )
TTF   DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
TTF   DISPLAY  CFA FAC1 NUMERO ORDEN          (AT APPEARANCE #    )
      *
      END LOOP EI
      *
      *
      IF       CFA PARAM FECHA CONTROL        NE CFA FAC1 FECHA TRANSACCION
T     WARNING  La fecha de la Fact. no Coincide con la del archivo Parametros
      *
      *        (X): Almacena la suma de los % de descuentos otorgados al client
      SET      --- X                          =      0
      *        99998 -> Crˆdito Empresas Afiliadas
      *        99995 -> Crˆdito Comercial Electrica (COMELSA)
      *
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99995
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99991
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99992
      OR       CFA FAC1 CODIGO CLIENTE        EQ     70005
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99
      OR       CFA FAC1 CODIGO CLIENTE        EQ     71275
F     SET      --- X                          =  CFA FAC1 PORCENTAJE DESCTO
F     COMPUTE  --- X                          +  CFA FAC1 PORCENTAJE DESCT1
F     IF       --- X                          GT     35
FT    ERROR    La Suma de los descuentos no puede ser mayor al 35 %
FT    BLINK    CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
FT    BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      *
      LABEL    NO VALIDAR DESCUENTO
      *
      IF       CFA FAC1 PORCENTAJE DESCT1     EQ
      OR       CFA FAC1 PORCENTAJE DESCT1     EQ     5
      OR       CFA FAC1 PORCENTAJE DESCT1     EQ     10
      OR       CFA FAC1 PORCENTAJE DESCT1     EQ     15
      OR       CFA FAC1 PORCENTAJE DESCT1     EQ     20
      OR       CFA FAC1 PORCENTAJE DESCT1     EQ     25
      OR       CFA FAC1 PORCENTAJE DESCT1     EQ     30
      OR       CFA FAC1 PORCENTAJE DESCT1     EQ     35
C     OR       CFA FAC1 PORCENTAJE DESCT1     EQ     40
F     ERROR    Descuento especial no es valido
F     BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
T     IF       CFA FAC1 CODIGO CLIENTE        NE     99999
T     AND      CFA FAC1 PORCENTAJE DESCT1     GT     0
TT    BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
TT    BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
TT    ERROR    Los Descuentos Apartes son unicamente para Ventas al Contado
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
T     IF       CFA FAC1 CODIGO CLIENTE        NE     7500
T     AND      CFA FAC1 PORCENTAJE DESCT1     GT     0
TT    BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
TT    BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
TT    ERROR    Este Cliente no admite descuento aparte llame a Computo!
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     3
T     IF       CFA FAC1 CALCULO IMPUESTO      EQ     0
T     AND      CFA FAC1 PORCENTAJE DESCT1     NE     0
TT    ERROR    Las Ventas EXCENTAS no admiten descuentos apartes llame a RHODA
TT    BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 CODIGO CLIENTE        EQ     99999
T     ERROR    Para una Venta Credito el Cliente debe ser Distinto de 99999
T     BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
T     BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 CODIGO CLIENTE        EQ     7500
T     ERROR    Para una Venta Credito el Cliente debe ser Distinto de 7500
T     BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
T     BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
      *
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99995
      AND      CFA FAC1 TIPO TRANSACCION      EQ     03
T     ERROR    Altensa solo se factura al Credito, cambie su transaccion
      *
      TRAP
      *
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     3
CT    IF       CFA FAC1 CODIGO CLIENTE        GE     7000
CTF   BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
CTF   BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
CTF   ERROR    El Codigo de Cliente para una Venta Contado debe ser 99999
      *
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     2
      AND      CFA FAC1 NOMBRE CLIENTE        EQ     CLIENTE CONTADO
T     ERROR    Toda transaccion CONTADO debe llevar un nombre de Cliente
      *
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     2
      AND      CFA FAC1 PORCENTAJE DESCT1     GT     0
T     BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
T     ERROR    Los Descuentos Apartes son recuperados con Notas de Debitos
      *
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     2
      AND      CFA FAC1 OBSERVACIONES         EQ
T     BLINK    CFA FAC1 OBSERVACIONES         (AT APPEARANCE #    )
T     ERROR    Registre el motivo de la Devolucion
      *
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     4
C     OR       CFA FAC1 TIPO TRANSACCION      EQ     5
CT    SET      CFA FAC1 PORCENTAJE DESCTO     =
CT    SET      CFA FAC1 PORCENTAJE DESCT1     =
CT    NO INPUT CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
CT    NO INPUT CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
CT    DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
CT    DISPLAY  CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      *
      *        Nueva Validacion
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     1
T     NO INPUT CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     2
T     OK INPUT CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
*     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
*     OR       CFA FAC1 TIPO TRANSACCION      EQ     3
*     OR       CFA FAC1 TIPO TRANSACCION      EQ     6
*T    NO INPUT CFA FAC1 APLICAR A             (AT APPEARANCE #    )
*     IF       CFA FAC1 TIPO TRANSACCION      EQ     2
*     OR       CFA FAC1 TIPO TRANSACCION      EQ     4
C     OR       CFA FAC1 TIPO TRANSACCION      EQ     5
*T    OK INPUT CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
      *        Si la factura ha sido grabada con renglones y se intenta modifi-
      *        car el tipo de transaccion validamos este cambio
      *
      *
      IF       --- M                          EQ     1
      AND      --- INTERACTIVE PHASE          EQ     DATA MODIFICATION
T     NO INPUT CFA FAC1 NO BODEGA             (AT APPEARANCE #    )
      *
      IF       --- M                          EQ     1
T     IF       CFA FAC1 TIPO TRANSACCION      NE --- N
TT    BLINK    CFA FAC1 TIPO TRANSACCION      (AT APPEARANCE #    )
TT    ERROR    Si Ud. quiere cambiar de transaccion, debe eliminar y refacturar
      *
      *        Validacion del Numero del Vendedor
      *
      SET      CFA VENDEDOR CODIGO            =  CFA FAC1 CODIGO VENDEDOR
      READ     CFA VENDEDOR               HOLD 0 FT 0 BY VENDEDOR CODIGO
F     BLINK    CFA FAC1 CODIGO VENDEDOR       (AT APPEARANCE #    )
F     ERROR    Codigo del Vendedor no Existe en el Archivo
      *
      *
      IF       CFA FAC1 SUCURSAL              EQ
      OR       CFA FAC1 SUCURSAL              EQ     0
T     ERROR    El Numero de Sucursal debe ser distinto de cero o blancos
T     BLINK    CFA FAC1 SUCURSAL              (AT APPEARANCE #    )
      *
      *        Validacion del Numero del Departamento
      *
      *        (W): Almacena el # del departamento
      *
*     IF       CFA FAC1 DEPARTAMENTO          EQ     1
*     OR       CFA FAC1 DEPARTAMENTO          EQ     2
*F    BLINK    CFA FAC1 DEPARTAMENTO          (AT APPEARANCE #    )
*F    ERROR    El Numero del Departamento debe ser "1" o "2"
*T    SET      --- W                          =  CFA FAC1 DEPARTAMENTO
*T    SET      CFA WORK DEPTO SAL             =  CFA WORK DEPARTAMENTO      W
*T    DISPLAY  CFA WORK DEPTO SAL             (AT APPEARANCE #    )
*T    BRIGHT   CFA WORK DEPTO SAL             (AT APPEARANCE #    )
      *
C     IF       CFA FAC1 DEPARTAMENTO          EQ     2
C     AND      CFA FAC1 NO BODEGA             EQ     1
CT    BLINK    CFA FAC1 DEPARTAMENTO          (AT APPEARANCE #    )
CT    ERROR    El Departamento NO. 2 esta asginado para el deposito de INCORSA
      *
      *        (I): Almacena el # de transaccion
      *
      SET      --- I                          =  CFA FAC1 TIPO DOCUMENTO
      SET      CFA WORK TIPO TRANSAC SAL      =  CFA WORK TIPO TRANSAC      I
      DISPLAY  CFA WORK TIPO TRANSAC SAL      (AT APPEARANCE #    )
      BRIGHT   CFA WORK TIPO TRANSAC SAL      (AT APPEARANCE #    )
      *
      IF       CFA FAC1 FECHA TRANSACCION     GT
F     SET DATE CFA FAC1 FECHA TRANSACCION
      DISPLAY  CFA FAC1 FECHA TRANSACCION     (AT APPEARANCE #    )
      *
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     1
*     OR       CFA FAC1 TIPO DOCUMENTO        EQ     3
T     SET      CFA FAC1 APLICAR A             =  CFA FAC1 NO DOCUMENTO
T     DISPLAY  CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     2
      AND      CFA FAC1 CODIGO CLIENTE        EQ     99995
T     GOTO     SALTA VALIDACION
      *
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     2
T     IF       CFA FAC1 APLICAR A             EQ CFA FAC1 NO DOCUMENTO
TT    IF       CFA FAC1 CODIGO CLIENTE        EQ     99999
TTF   ERROR    Registre el No. de Factura que hara referencia la Dev. CREDITO
TTF   BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
TTT   ERROR    Registre el No. de Factura que hara referencia la Dev. CONTADO
TTT   BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
      *
      IF       CFA FAC1 TIPO DOCUMENTO        EQ     2
      OR       CFA FAC1 TIPO DOCUMENTO        EQ     3
T     IF       CFA FAC1 APLICAR A             EQ CFA FAC1 NO DOCUMENTO
TT    BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
TT    ERROR    Las Notas Debitos o Creditos Necesitan Documento de referencia
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      OR       CFA FAC1 TIPO TRANSACCION      EQ     2
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
      *
      STORE    CFA FACTURA1                   RECORD
      *
      *        Busqueda del documento en el archivo de Transacciones de CXC
      *        Cuando es Credito o Contado
CT    SET      --- I                          =      0
CT    SET      CAR TRANSACT CUSTOMER NO       =  CFA FAC1 CODIGO CLIENTE
CT    BEG AT   CAR TRANSACT IN CAR FAC1 CODIGO CLIENTE
CT    END AT   CAR TRANSACT IN CAR FAC1 CODIGO CLIENTE
CT    BEG READ CAR TRANSACT               HOLD 0 KEY IS  TRANSACT KEY
CT    IF       CAR TRANSACT CUSTOMER NO       EQ CFA FAC1 CODIGO CLIENTE
CT    AND      CAR TRANSACT ORDER NO          EQ CFA FAC1 NUMERO ORDEN
CT    GOTO     DEJA DE LEER TRANSACT
CTT   SET      --- I                          =      1
CTT   SET      --- J                          =  CAR TRANSACT TRX TYPE
CTT   SET      --- S                          =  CAR TRANSACT PORC DESCTO
CTT   SET      --- R                          =  CAR TRANSACT DESCTO APARTE
CTT   SET      CFA WORK ORDEN DE COMPRA       =  CAR TRANSACT ORDER NO
CTT   SET      CFA WORK MONTO FACTURA         =  CAR TRANSACT AMOUNT
CT    END READ CAR TRANSACT
      *
      TRAP
      *        Cuando se trata de una Devolucion
      *
F     SET      --- I                          =      0
F     SET      CAR TRANSACT CUSTOMER NO       =  CFA FAC1 CODIGO CLIENTE
F     SET      CAR TRANSACT APPLY TO          =  CFA FAC1 APLICAR A
F     BEG AT   CAR TRANSACT IN CAR TRANSACT APPLY TO KEY
F     END AT   CAR TRANSACT IN CAR TRANSACT APPLY TO KEY
F     BEG READ CAR TRANSACT               HOLD 0 KEY IS  TRANSACT KEY
F     IF       CAR TRANSACT CUSTOMER NO       EQ CFA FAC1 CODIGO CLIENTE
FT    SET      --- I                          =      1
FT    SET      --- J                          =  CAR TRANSACT TRX TYPE
FT    SET      --- S                          =  CAR TRANSACT PORC DESCTO
FT    SET      --- R                          =  CAR TRANSACT DESCTO APARTE
FT    SET      CFA WORK MONTO FACTURA         =  CAR TRANSACT AMOUNT
F     END READ CAR TRANSACT
      *
      *        Aqui agregar LABEL : DEJA DE LEER TRANSACT
      *
      RESTORE  CFA FACTURA1                   RECORD
      *
CT    IF       --- I                          EQ     1
CTT   IF       CFA FAC1 NUMERO ORDEN          EQ CFA WORK ORDEN DE COMPRA
CTTT  MESSAGE  OJO-->  ESTA ORDEN DE COMPRA YA FUE DESPACHADA CON ANTERIORIDAD
      *
      *
F     IF       --- I                          EQ     1
FT    IF       CFA FAC1 PORCENTAJE DESCTO     NE --- S
FTT   WARNING  % Descto de la Devolucion No es Igual al de la Factura Verifique
FTT   BLINK    CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
FT    IF       CFA FAC1 PORCENTAJE DESCT1     NE --- R
FTT   WARNING  % Descto Aparte No Es Igual al de La Factura Original Verifique
FTT   BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      GOTO     EQUIVOCADO
FT    SET      CFA FAC1 PORCENTAJE DESCTO     =  --- S
FT    SET      CFA FAC1 PORCENTAJE DESCT1     =  --- R
FT    DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
FT    DISPLAY  CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      LABEL    EQUIVOCADO
      *
F     IF       --- I                          EQ     1
*F    AND      CFA FAC1 TIPO TRANSACCION      EQ     1
*F    OR       CFA FAC1 TIPO TRANSACCION      EQ     3
F     AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
F     AND      --- J                          EQ     1
F     OR       --- J                          EQ     5
FT    WARNING  Numero de Factura Aplicar a ya Existe
FT    BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
F     SET      CFA WORK CLIENTE               =  CFA FAC1 CODIGO CLIENTE
F     SET      --- TEMP 9                     =  CFA FACTURA1 FORMA PAGO
      *
      *        Busqueda del documento en el archivo del encabezado de Facturas
      *
      STORE    CFA FACTURA1                   RECORD
      *        ---Ernesto 03/02/2017
      TRAP
      *
F     IF       --- I                          EQ     0
FT    SET      CFA FAC1 NO DOCUMENTO          =  CFA FAC1 APLICAR A
FT    BEG AT   CFA FACTURA1 IN CFA FAC1 NO DOCUMENTO
FT    END AT   CFA FACTURA1 IN CFA FAC1 NO DOCUMENTO
FT    BEG READ CFA FACTURA1               HOLD 0 KEY IS  FAC1 NO DOCUMENTO
      *
FT    IF       CFA WORK CLIENTE               EQ CFA FAC1 CODIGO CLIENTE
      *        ----------------------------------------------------------------
FTT   SET      --- I                          =      1
FTT   SET      --- R                          =  CAR TRANSACT DESCTO APARTE
FTT   SET      --- S                          =  CAR TRANSACT PORC DESCTO
      *
FTF   ERROR    Cliente para la Devolucion no es igual al de la Factura Original
FTF   BLINK    CFA FAC1 CODIGO CLIENTE        (AT APPEARANCE #    )
FTF   BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
FT    END READ CFA FACTURA1
      *
      *
FT    RESTORE  CFA FACTURA1                   RECORD
      *
F     IF       --- I                          EQ     1
FT    IF       CFA FAC1 PORCENTAJE DESCTO     NE --- S
FTT   WARNING  % Descto de la Devolucion No es Igual al de la Factura Verifique
FTT   BLINK    CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
FT    IF       CFA FAC1 PORCENTAJE DESCT1     NE --- R
FTT   WARNING  % Descto Aparte No Es Igual al de La Factura Original Verifique
FTT   BLINK    CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      GOTO     EQUIVOCADOS
FT    SET      CFA FAC1 PORCENTAJE DESCTO     =  --- S
FT    SET      CFA FAC1 PORCENTAJE DESCT1     =  --- R
FT    DISPLAY  CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
FT    DISPLAY  CFA FAC1 PORCENTAJE DESCT1     (AT APPEARANCE #    )
      LABEL    EQUIVOCADOS
      *
      *
FF    IF       --- I                          EQ     0
FF    AND      CFA FAC1 TIPO DOCUMENTO        EQ     3
FFT   WARNING  La Factura que aplica esta NOTA DEBITO no existe en los archivos
FFT   BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
FF    IF       --- I                          EQ     0
FF    AND      CFA FAC1 TIPO TRANSACCION      EQ     5
FFT   WARNING  LA Factura que Aplica Esta Nota Credito NO Existe en Archivos
FFT   BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
      *
      *        Validar Vendedores q puedes Hacer N/Credito Viejas
FF    IF       --- I                          EQ     0
FF    AND      CFA FAC1 TIPO DOCUMENTO        EQ     2
FFT   SET      CIC WORK CURRENT USER ID       =  --- USER ID
FFT   IF       CIC WORK CURRENT USER ID       EQ     RAF
FFT   OR       CIC WORK CURRENT USER ID       EQ     ECB
FFT   OR       CIC WORK CURRENT USER ID       EQ     AMV
FFT   OR       CIC WORK CURRENT USER ID       EQ     IAR
FFT   OR       CIC WORK CURRENT USER ID       EQ     SPA
FFT   OR       CIC WORK CURRENT USER ID       EQ     IYM
FFT   OR       CIC WORK CURRENT USER ID       EQ     EAM
FFT   OR       CIC WORK CURRENT USER ID       EQ     WGB
FFT   OR       CIC WORK CURRENT USER ID       EQ     LAD
FFT   OR       CIC WORK CURRENT USER ID       EQ     FQN
FFT   OR       CIC WORK CURRENT USER ID       EQ     EVE
FFT   OR       CIC WORK CURRENT USER ID       EQ     JRM
FFT   OR       CIC WORK CURRENT USER ID       EQ     CES
FFT   OR       CIC WORK CURRENT USER ID       EQ     EYM
FFT   OR       CIC WORK CURRENT USER ID       EQ     EJJ
FFT   OR       CIC WORK CURRENT USER ID       EQ     OER
FFT   OR       CIC WORK CURRENT USER ID       EQ     JJE
FFT   OR       CIC WORK CURRENT USER ID       EQ     LGF
FFT   OR       CIC WORK CURRENT USER ID       EQ     RAB
FFT   OR       CIC WORK CURRENT USER ID       EQ     NYP
FFT   OR       CIC WORK CURRENT USER ID       EQ     RJM
FFTF  BLINK    CFA FAC1 APLICAR A             (AT APPEARANCE #    )
FFTF  ERROR    La Factura que aplica esta Devolucion no existe en los Archivos
      *
      *        Cuando No Hay Amarre Para Leer TRANSACT y FACTURA1
      *        Validacion de limites de creditos, y antiguedades de saldos
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 CODIGO CLIENTE        EQ     99995
T     GOTO     SALTA VALIDACION
      *
      TRAP
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     1
      AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
T     TRAP
      *
T     SET      CAR CUSTOMER NO                =  CFA FAC1 CODIGO CLIENTE
T     READ     CAR CUSTOMER               HOLD 0 FT 0 BY CUSTOMER NO
TT    SET      CAR WORK CREDIT LIMIT          =  CAR CUSTOMER CREDIT LIMIT
TT    SET      --- R                          =  CAR CUSTOMER CREDIT LIMIT
TT    SET      --- E                          =  CAR CUSTOMER CREDIT LIMIT
TT    IF       CAR CUSTOMER POST RECEIV       EQ     1
TTT   IF       CAR CUSTOMER NO                EQ     99991
TTT   OR       CAR CUSTOMER NO                EQ     99990
TTTF  SET      CFA FAC1 NOMBRE CLIENTE        =  CAR CUSTOMER NAME
TTF   ERROR    Este cliente esta asignado a ventas contado solamente
TT    COMPUTE  CAR CUSTOMER CREDIT LIMIT      -  CAR CUSTOMER BALANCE
TT    IF       CAR CUSTOMER CREDIT LIMIT      LT     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TTT   ERROR    Cliente con Limite de Credito Excedido, Consulte al Dpto Credito
TT    IF       CAR CUSTOMER CREDIT LIMIT      EQ     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TTT   ERROR    Cliente no tiene Saldo Disponible, Consulte al Dpto. de Credito
      *
TT    IF       CAR CUSTOMER MOROSIDAD     005 GT     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TT    AND      CAR CUSTOMER TERMS CODE        LE     120
TTT   BLINK    CFA FAC1 NOMBRE CLIENTE        (AT APPEARANCE #    )
TTT   ERROR    Cliente con Saldo a mas de 120 Dias,Solicite Clave de Aprobacion
TT    IF       CAR CUSTOMER MOROSIDAD     004 GT     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TT    AND      CAR CUSTOMER TERMS CODE        LT     90
TTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TTT   ERROR    Cliente Con Saldo a mas de 90 Dias, Solicite Clave de Aprobacion
      *
TF    ERROR    Numero de Cliente No Existe en el Archivo de Clientes
      *
      *        Validacion para PD Creditos 12/07/2112
      *
      IF       CFA FAC1 TIPO TRANSACCION      EQ     6
      AND      CFA FACTURA1 FORMA PAGO        EQ     CREDITO
      *
T     SET      CAR CUSTOMER NO                =  CFA FAC1 CODIGO CLIENTE
T     READ     CAR CUSTOMER               HOLD 0 FT 0 BY CUSTOMER NO
TT    SET      CAR WORK CREDIT LIMIT          =  CAR CUSTOMER CREDIT LIMIT
TT    SET      --- R                          =  CAR CUSTOMER CREDIT LIMIT
TT    SET      --- E                          =  CAR CUSTOMER CREDIT LIMIT
TT    IF       CAR CUSTOMER POST RECEIV       EQ     1
TTT   IF       CAR CUSTOMER NO                EQ     99991
TTT   OR       CAR CUSTOMER NO                EQ     99990
TTTF  SET      CFA FAC1 NOMBRE CLIENTE        =  CAR CUSTOMER NAME
TTF   ERROR    Este cliente esta asignado a ventas contado solamente
TT    COMPUTE  CAR CUSTOMER CREDIT LIMIT      -  CAR CUSTOMER BALANCE
TT    IF       CAR CUSTOMER CREDIT LIMIT      LT     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TTT   ERROR    Cliente con Limite de Credito Excedido, Consulte al Dpto Credito
TT    IF       CAR CUSTOMER CREDIT LIMIT      EQ     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TTT   ERROR    Cliente no tiene Saldo Disponible, Consulte al Dpto. de Credito
      *
TT    IF       CAR CUSTOMER MOROSIDAD     005 GT     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TT    AND      CAR CUSTOMER TERMS CODE        EQ     120
TTT   BLINK    CFA FAC1 NOMBRE CLIENTE        (AT APPEARANCE #    )
TTT   ERROR    Cliente con Saldo a mas de 120 Dias,Solicite Clave de Aprobacion
TT    IF       CAR CUSTOMER MOROSIDAD     004 GT     0
TT    AND      CFA FAC1 REFERN CREDITO        EQ
TT    AND      CAR CUSTOMER TERMS CODE        LT     90
TTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
TTT   ERROR    Cliente Con Saldo a mas de 90 Dias, Solicite Clave de Aprobacion
      *
      LABEL    SALTA VALIDACION
      *
      *
      IF       CAR CUSTOMER DUNNING CODE      EQ     2
T     BLINK    CFA FAC1 PORCENTAJE DESCTO     (AT APPEARANCE #    )
T     ERROR    El descuento para este cliente es dado a traves de N/CR
      *
      GOTO     XXXX
      IF       CAR CUSTOMER NO                NE     99999
      AND      CAR CUSTOMER NO                NE     99991
      AND      CAR CUSTOMER NO                NE     99990
      AND      CAR CUSTOMER NO                NE     99992
T     SET      CFA FAC1 NOMBRE CLIENTE        =  CAR CUSTOMER NAME
      DISPLAY  CFA FAC1 NOMBRE CLIENTE        (AT APPEARANCE #    )
      LABEL    XXXX
      *
      *        AQUI ES LO QUE ADICIONE
      *
C     IF       CFA FAC1 TIPO TRANSACCION      EQ     3
C     AND      CFA FAC1 CODIGO CLIENTE        NE     99999
CT    SET      CFA CONTADO NO                 =  CFA FAC1 CODIGO CLIENTE
CT    READ     CFA CONTADO NO             HOLD 0 FT 0 BY CONTADO NO
CTT   SET      CFA FAC1 NOMBRE CLIENTE        =  CFA CONTADO NAME
CTT   DISPLAY  CFA FAC1 NOMBRE CLIENTE        (AT APPEARANCE #    )
      *
      *
      *
      *
      SET      CFA WORK CONTINUACION          =
      APPEND   CFA WORK CONTINUACION          0      Presione ENTER Para
      APPEND   CFA WORK CONTINUACION          1      Continuar
      DISPLAY  CFA WORK CONTINUACION          (AT APPEARANCE #    )
      BRIGHT   CFA WORK CONTINUACION          (AT APPEARANCE #    )
      *
      IF       CFA FAC1 CODIGO CLIENTE        EQ     99999
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99998
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99991
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99990
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99995
      OR       CFA FAC1 CODIGO CLIENTE        EQ     99994
T     BLANK    CAR CUSTOMER CREDIT LIMIT      (AT APPEARANCE #    )
T     BEG LOOP T  = 001 TO 007  STEP 001
T     BLANK    CFA WORK NOMBRE MOROSIDAD  T   (AT APPEARANCE #    )
T     END LOOP T
T     BEG LOOP T  = 001 TO 005  STEP 001
T     BLANK    CFA WORK MOROSIDAD         T   (AT APPEARANCE #    )
T     END LOOP T
      *
      *
F     SET      CAR CUSTOMER CREDIT LIMIT      =  --- R
F     DISPLAY  CAR CUSTOMER CREDIT LIMIT      (AT APPEARANCE #    )
      *
F     SET      CFA WORK NOMBRE MOROSIDAD  001 =      Morosidad
F     SET      CFA WORK NOMBRE MOROSIDAD  002 =      Corriente
F     SET      CFA WORK NOMBRE MOROSIDAD  003 =       30 Dias
F     SET      CFA WORK NOMBRE MOROSIDAD  004 =       60 Dias
F     SET      CFA WORK NOMBRE MOROSIDAD  005 =       90 Dias
F     SET      CFA WORK NOMBRE MOROSIDAD  006 =      120 Dias
F     SET      CFA WORK NOMBRE MOROSIDAD  007 =         Saldo
      *
F     SET      CFA WORK MOROSIDAD         001 =  CAR CUSTOMER MOROSIDAD     001
F     SET      CFA WORK MOROSIDAD         002 =  CAR CUSTOMER MOROSIDAD     002
F     SET      CFA WORK MOROSIDAD         003 =  CAR CUSTOMER MOROSIDAD     003
F     SET      CFA WORK MOROSIDAD         004 =  CAR CUSTOMER MOROSIDAD     004
F     SET      CFA WORK MOROSIDAD         005 =  CAR CUSTOMER MOROSIDAD     005
      *
F     SET      CFA WORK MOROSIDAD         006 =
F     BEG LOOP O  = 001 TO 005  STEP 001
F     COMPUTE  CFA WORK MOROSIDAD         006 +  CFA WORK MOROSIDAD         O
F     END LOOP O
F     DISPLAY  CFA WORK MOROSIDAD         006 (AT APPEARANCE #    )
      *
F     BEG LOOP O  = 001 TO 005  STEP 001
F     DISPLAY  CFA WORK MOROSIDAD         O   (AT APPEARANCE #    )
F     END LOOP O
      *
F     BEG LOOP O  = 001 TO 007  STEP 001
F     DISPLAY  CFA WORK NOMBRE MOROSIDAD  O   (AT APPEARANCE #    )
F     END LOOP O
      *
F     IF       CFA FAC1 TIPO TRANSACCION      EQ     1
F     AND      CFA FAC1 TIPO DOCUMENTO        EQ     1
FT    COMPUTE  CAR CUSTOMER CREDIT LIMIT      -  CFA WORK MOROSIDAD         006
FT    IF       CAR CUSTOMER CREDIT LIMIT      LT     0
FT    AND      CFA FAC1 REFERN CREDITO        EQ
FTT   BLINK    CFA FAC1 REFERN CREDITO        (AT APPEARANCE #    )
FTT   ERROR    Cliente excede su Limite de CR, Solicite Aprobacion en Dpto Cred
FTT   END
      *
      *        Validar Devoluciones en CM
      READ     CFA TMPVENDE               HOLD 0 FT 0 BY
T     IF       CFA TMPVENDE_ID_USER           EQ --- USER ID
T     AND      CFA TMPVENDE_NO_VENDEDOR       EQ CFA FAC1 CODIGO VENDEDOR
TT    GOTO     NO VALIDAR N/CREDITO
      *
      IF       CFA FAC1 FORMA DE VENTA        NE     2
T     IF       --- INTERACTIVE PHASE          EQ     DATA ADDITION
TT    IF       CFA FAC1 TIPO DOCUMENTO        EQ     2
TTT   BLINK    CFA FAC1 TIPO DOCUMENTO        (AT APPEARANCE #    )
TTT   ERROR    << Las Notas de Creditos Son Gestinadas por el WMS >>
TTT   END
      *
      LABEL    NO VALIDAR N/CREDITO
